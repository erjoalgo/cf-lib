(in-ns 'cf-lib.core)

(defn cf-curl
  "retrying wrapper around clj-http.client requests
  refreshes token and retries request on 401 status code"
  [cf-target path & {:keys [verb query-params body
                            extra-http-client-args retry-count]
                     :or {verb :GET retry-count 0}}]

  (log/infof "cf curl is %s %s\n" (name verb) path)
  (let [verb-fun (case verb
                   :GET client/get
                   :POST client/post
                   :DELETE client/delete
                   :PUT client/put)

        url (str (:api-endpoint cf-target) path)

        proxy-map (proxy-to-map (:proxy cf-target))
        token (or (-> :token cf-target)
                  (token-for-cf-target! cf-target
                                        :force (> retry-count 0)))
        token-header {"Authorization" (str "bearer " token)}
        update-args {:headers (partial merge token-header)
                     :body (fnil identity (json/write-str body))
                     :query-params (partial merge query-params)
                     :proxy-host (fnil identity (:host proxy-map))
                     :proxy-port (fnil identity (:port proxy-map))
                     :insecure? (fnil identity (:insecure? cf-target))}
        complete-client-args (reduce-kv update extra-http-client-args
                                        update-args)
        ]
    (try (verb-fun url complete-client-args)
         (catch Exception ex
           (if-not
               (and (< retry-count 2)
                    ;;TODO check if has .getData method
                    (-> ex .getData :status (= 401)))
             (throw ex)
             (cf-curl cf-target path
                      :verb verb
                      :http-client-args extra-http-client-args
                      :retry-count (inc (or retry-count 0))))))))

(defn cf-extract-name
  "extract name from a cf response json"
  [resp]
  (reduce get resp ["entity" "name"]))

(defn cf-extract-guid
  "extract guid from a cf response json"
  [resp]
  (reduce get resp ["metadata" "guid"]))

(defn cf-extract-url
  "extract url from a cf response json"
  [resp]
  (reduce get resp ["metadata" "url"]))

(defn cf-extract-entity-field
  "extract field from a cf response json's entity sub-map"
  [field resp]
  (reduce get resp ["entity" field]))

(defn cf-depaginate-resources
  "takes a paginated response, fetches all pages
  and accumulates their resources. awkard implementation"
  [cf-target resp]
  (->>
   resp
   (iterate (comp (fnil (comp
                         json/read-str
                         :body
                         (partial cf-curl cf-target)) nil)
                  (get resp "next_url"))
            (-> resp json/read-str :body))
   (take-while (comp not nil?))
   (mapcat #(get % "resources"))))

(defn cf-depaginate-resources
  "takes a paginated response, fetches all pages
  and accumulates their resources. sequential"
  [cf-target resp-body]
  (loop [resp-body resp-body
         resources []
         page 0]
    (log/tracef "on page %s\n" page)
    (if-not resp-body
      (apply concat resources)
      (let [resp-body (-> resp-body
                          :body
                          json/read-str)
            next-url (get resp-body "next_url")
            new-resources (get resp-body "resources")]
        (recur (and next-url (cf-curl cf-target next-url))
               (conj resources new-resources)
               (inc page))))))

(defn cf-pdepaginate-resources
  "takes a paginated response, fetches all pages
  and accumulates their resources. parallel depagination"
  [cf-target first-resp]
  (let [first-resp-json (-> first-resp :body json/read-str)
        total-pages (get first-resp-json "total_pages")
        sample-next-url (get first-resp-json "next_url")
                                        ;ith-page-url #(clojure.string/replace
                                        ;sample-next-url
                                        ;#"page=([0-9]+)"
                                        ;(format "page=%d" %))
        page-indices (range 2 (inc total-pages))
        pages (->> page-indices
                   (pmap (comp
                          json/read-str
                          :body
                          #(cf-curl cf-target
                                    sample-next-url
                                    :query-params {:page %}))))

        all-pages (conj pages first-resp-json)
        ]
    (->> all-pages
         (map (comp #(get % "resources")))
         (apply concat))))
