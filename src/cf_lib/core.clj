(ns cf-lib.core
  (:require
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   )
  (:gen-class))

(defmacro get-chain [obj & accessors]
  "(get-chain {1 {2 {3 {4 5}}}} 1 2 3) => {4 5}"
  ;;the same can be accomplished without a macro:
  ;;(reduce get {1 {2 {3 {4 5}}}} [1 2 3]) => {4 5}"
  (if (empty? accessors) obj
      `(get-chain (get ~obj ~(first accessors)) ~@(rest accessors))))

(defn proxy-map [proxy]
  "make sure proxy is a {:host host :port port} map
and not a string like http://my-proxy:8080"
  (when proxy
    (condp instance? proxy
      java.util.Map proxy
      String (let [[_ host port] (re-matches #"^(.*):([0-9]+)$" proxy)]
               {:host host
                :port port}))))

(defn cf-token [cf-target]
  (let [username (:user cf-target)
        password (:pass cf-target)
        login-endpoint (clojure.string/replace
                        (:api-endpoint cf-target)
                        #"api" "login")
        proxy-map (proxy-map (:proxy cf-target))
        resp (client/post (str login-endpoint "/oauth/token")
                          {:basic-auth ["cf" ""]
                           :form-params {:username username
                                         :password password
                                         :grant_type "password"}
                           :accept :json
                           :proxy-host (:host proxy-map)
                           :proxy-port (:port proxy-map)
                           :insecure? (:insecure? cf-target)})
        body-json (json/read-str (:body resp))
        oauth-token (-> resp :body json/read-str
                        (get "access_token"))]
    (log/infof "token obtained: %s"  oauth-token)
    ;;(reset! (get cf-target :oauth-token) oauth-token)
    oauth-token))

(def cf-target-to-token
  "map cf targets to tokens accros threads"
  (atom {}))

(defn token-for-cf-target! [cf-target & {:keys [force]}]
  "returns a token for cf-target. if no token exists or if
:force is true obtain a new token and associate it with target"
  (let [existing (and (not force)
                      (get @cf-target-to-token cf-target))]
    ;;TODO ensure anyone reading ref is BLOCKED until token is obtained?
    (or existing (get (swap! cf-target-to-token
                        assoc
                        cf-target
                        (cf-token cf-target)) cf-target))))

(defn cf-curl [cf-target path
               & {:keys [verb
                         query-params
                         body
                         extra-http-client-args retry-count]
                  :or {verb :GET
                       retry-count 0}}]
  (log/tracef "cf curl is %s %s\n" (name verb) path)
  (let [verb-fun (case verb
                   :GET client/get
                   :POST client/post
                   :DELETE client/delete
                   :PUT client/put)

        url (str (:api-endpoint cf-target) path)

        proxy-map (proxy-map (:proxy cf-target))
        token-header {"Authorization"
                      (->> (token-for-cf-target! cf-target
                                                 :force (> retry-count 0))
                           (str "bearer "))}

        update-in-args {[:headers] (partial merge token-header)
                        [:body] (fnil identity (json/write-str body))
                        [:query-params] (partial merge query-params)
                        [:proxy-host] (fnil identity (:host proxy-map))
                        [:proxy-port] (fnil identity (:port proxy-map))
                        [:insecure?] (fnil identity (:insecure? cf-target))}
        complete-client-args (reduce-kv update-in extra-http-client-args
                                        update-in-args)
        ]
    '(printf "complete client args: %s %s %s\n"
            verb url
            complete-client-args)
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

(defn cf-depaginate-resources [cf-target resp]
  (->>
   resp
   (iterate (comp (fnil (comp
                         json/read-str
                         :body
                         (partial cf-curl cf-target)) nil)
                  (get resp "next_url"))
            (-> resp json/read-str :body))
   (take-while (comp not nil?))
   (map #(get % "resources"))
   (reduce concat)))

(defn cf-depaginate-resources [cf-target resp-body]
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

(defn cf-pdepaginate-resources [cf-target first-resp]
  "parallel depagination"
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

        all-pages (conj pages first-resp)
        ]
    (->> all-pages
         (map (comp #(get % "resources")))
         (apply concat))))

(defmacro cf-define-depaginating-functions [& name-url-pairs]
  "for each (fun-name url), define a cf function
 that takes a cf-target and optionally a guid, and make a cf-curl
call"
  `(do ~@(map (fn [[fun-name-sym url]]
         (let [needs-format (.contains url "%s")
               cf-target-sym (gensym "cf-target-")
               guid-sym (gensym "guid-")
               arg-list (apply vector cf-target-sym
                               (when needs-format [guid-sym]))
               url-form (if needs-format
                          `(format ~url ~guid-sym)
                          url)
               ]
           `(defn ~fun-name-sym ~arg-list
              (->> ~url-form
                   (cf-curl ~cf-target-sym)
                   (cf-pdepaginate-resources ~cf-target-sym)))
           ))
       name-url-pairs)))

(cf-define-depaginating-functions
 [cf-apps "/v2/apps"]
 [cf-app-routes "/v2/apps/%s/routes"]
 [cf-app-bindings "/v2/apps/%s/service_bindings"]

 [cf-service-instances "/v2/service_instances"]
 [cf-service-instance-bindings "/v2/service_instances/%s/service_bindings"]

 [cf-spaces "/v2/spaces"]
 [cf-space-apps "/v2/spaces/%s/apps"]
 [cf-space-service-instances "/v2/spaces/%s/service_instances"]

 [cf-space-services "/v2/spaces/%s/services"]
 [cf-space-service-brokers "/v2/spaces/%s/service_brokers"]

 [cf-routes "/v2/routes"]
 [cf-route-apps "/v2/routes/%s/apps"]

 [cf-services "/v2/services"]
 [cf-service-plans "/v2/services/%s/service_plans"]
 [cf-service-brokers "/v2/service_brokers"]

 [cf-users "/v2/users"]
 [cf-orgs "/v2/organizations"]
 )

(defn format-sym [fmt & strs-or-syms]
  (->> strs-or-syms
       (map name)
       (apply format fmt)
       symbol))

(defmacro cf-define-get-delete-functions
  [& name-url-pairs]
  "for each (fun-name url) pair, define
1. a getter function that takes a cf-target and a guid,
2. a deleter for the same resource"
  `(do
      ~@(->> (map (fn [[cf-fun url]]
                  (let [subs-count (-> (re-seq #"%s" url) count)
                        gen-guid-syms (fn []
                                        (repeatedly subs-count
                                                    #(gensym "guid-")))

                        guid-syms-get (gen-guid-syms)
                        guid-syms-delete (gen-guid-syms)

                        cf-fun-delete (format-sym "%s-delete" cf-fun)

                        cf-target-get (gensym "cf-target-")
                        cf-target-delete (gensym "cf-target-")
                        ]

                    [
                     `(defn ~cf-fun ~(apply vector
                                           cf-target-get
                                           guid-syms-get)
                        (let [url# (format ~url ~@guid-syms-get)]
                          (-> (cf-curl ~cf-target-get url#)
                              :body json/read-str)))

                     `(defn ~cf-fun-delete ~(apply vector
                                                  cf-target-delete
                                                  guid-syms-delete)
                        (let [url# (format ~url ~@guid-syms-delete)]
                          (-> (cf-curl ~cf-target-delete url#
                                       :verb :DELETE))))
                     ]
                    ))
                name-url-pairs)
           (reduce concat)
           do)))



(cf-define-get-delete-functions
 [cf-app "/v2/apps/%s"]
 [cf-space "/v2/spaces/%s"]
 [cf-app-binding "/v2/apps/%s/service_bindings/%s"]
 [cf-service-binding "/v2/service_bindings/%s"]
 [cf-service-instance "/v2/service_instances/%s"]
 [cf-service-plan "/v2/service_plans/%s"]
 [cf-service "/v2/services/%s"]
 [cf-route "/v2/routes/%s"]
 [cf-org "/v2/organizations/%s"]
 )

(defn cf-extract-name [resp]
  (reduce get resp ["entity" "name"]))

(defn cf-extract-guid [resp]
  (reduce get resp ["metadata" "guid"]))

(defn cf-extract-entity-field [field resp]
  (reduce get resp ["entity" field]))

(defmacro cf-define-to-from-name-functions [& cf-fun-syms]
  "for each cf-fun, define functions,
1. $(cf-fun)-by-name to lookup by name
2. $(cf-fun)-name, to extract the name (returns a string)
3. $(cf-fun)-name-to-guid, to extract the guid (returns a string)

cf-fun-sym must be an existing function
"
  `(do ~@(->> (map (fn [cf-fun-sym]
                     (let [find-by-name-sym (format-sym "%s-by-name" cf-fun-sym)
                           cf-fun-sym-plural (format-sym "%ss" cf-fun-sym)
                           to-name-sym (format-sym "%s-name" cf-fun-sym)
                           name-to-guid-sym (format-sym "%s-name-to-guid"
                                                        cf-fun-sym)
                           ]
                       [
                        `(defn ~to-name-sym [cf-target# guid#]
                           (-> (~cf-fun-sym cf-target# guid#)
                               (cf-extract-name)))

                        `(defn ~find-by-name-sym [cf-target# name#]
                           (let [matches#
                                 (->> (~cf-fun-sym-plural cf-target#)
                                      (filter (comp (partial = name#)
                                                    cf-extract-name)))
                                 ]
                             (when (rest matches#)
                               (log/warnf
                                ~(format
                                  "multiple matches for %s with name: %%s"
                                  cf-fun-sym) name#))
                             (first matches#)))

                        `(defn ~name-to-guid-sym [cf-target# name#]
                           (-> (~find-by-name-sym cf-target# name#)
                               cf-extract-guid))
                        ]
                       ))
                   cf-fun-syms)
              (reduce concat))))

(cf-define-to-from-name-functions
 cf-app
 cf-space
 cf-service-instance)

(defmacro cf-define-resource-all-delete [& resource-deleter-pairs]
  "define a function to delete all resources.
 args is a list of (resource, deleter) pairs
 corresponding to existing cf functions.
resources should retrieve all resources.
deleter deletes a single resource
"
  `(do ~@(->> resource-deleter-pairs
        (map (fn [[resource deleter]]
               (let [all-deleter-sym (format-sym "%s-delete" resource)]
                 (printf "res del all %s %s %s\n"
                         resource deleter all-deleter-sym)
                 `(defn ~all-deleter-sym [cf-target# guid#]
                    (->> (~resource cf-target# guid#)
                         (pmap (comp (partial ~deleter cf-target#)
                                    cf-extract-guid))
                         dorun))
                 )))
        )))

(cf-define-resource-all-delete
 [cf-app-bindings cf-app-binding-delete]
 [cf-service-instance-bindings cf-service-binding-delete]
 [cf-app-routes cf-route-delete])

(defn cf-app-delete-force [cf-target app-guid]
  (do
    (cf-app-bindings-delete cf-target app-guid)
    (cf-app-delete cf-target app-guid)
    (cf-app-routes-delete cf-target app-guid)))

(defn cf-service-instance-delete-force [cf-target service-instance-guid]
  (do (cf-service-instance-bindings-delete cf-target service-instance-guid)
      (cf-service-instance-delete cf-target service-instance-guid)))

(defn cf-service-instance-create [cf-target name service-plan-guid space-guid
                                  & {:keys [payload]}]
  (cf-curl cf-target "/v2/service_instances" :verb :POST
           :body {"name" name
                  "service_plan_guid" service-plan-guid
                  "space_guid" space-guid
                  "parameters" payload}))

(defn cf-service-instance-create-human [cf-target name
                                        service-label
                                        service-plan-name
                                        space-name
                                        & {:keys [payload]}]
  (let [service (cf-service-by-label cf-target service-label)
        service-guid (cf-extract-guid service)
        service-plan (cf-service-plan-by-name
                      cf-target
                      service-guid
                      service-plan-name)
        service-plan-guid (cf-extract-guid service-plan)
        space-guid (cf-space-name-to-guid cf-target space-name)]
    (cf-service-instance-create
     cf-target
    ;(vector
     name service-plan-guid space-guid :payload payload)))

(defn cf-get-envs [cf-target app-guid]
  (-> (cf-curl cf-target
               (format "/v2/apps/%s/env" app-guid))
      :body json/read-str))

(defn cf-set-envs [cf-target app-guid envs]
  (let [current-envs (cf-get-envs cf-target app-guid)]
    (cf-curl
     cf-target
     (format "/v2/apps/%s" app-guid)
     :verb :PUT
     :body {:environment_json (merge current-envs envs)})))

(defn cf-app-vcap-services [cf-target app-guid]
  (-> (cf-get-envs cf-target app-guid)
      (get "system_env_json")))

(defn cf-service-by-label [cf-target service-label]
  (->> (cf-services cf-target)
      (filter (comp (partial = service-label)
                    (partial cf-extract-entity-field "label")))
      first))

;;redefine this function: it makes no sense to search across all services
;;for a plan by given name, ie a lot of duplicates, "Free", "Tiered", etc
(defn cf-service-plan-by-name [cf-target service-guid service-plan-name]
  (->> (cf-service-plans cf-target service-guid)
       (filter (comp (partial = service-plan-name)
                     cf-extract-name))
       first))

(defn cf-service-instances-by-service-label [cf-target service-label]
  (let [service-guid (-> (cf-service-by-label cf-target service-label)
                    cf-extract-guid)
        service-plan-guids (->> (cf-service-plans cf-target service-guid)
                                (map cf-extract-guid))]
    (->> (cf-service-instances cf-target)
         (filter (comp (partial contains? (set service-plan-guids))
                       (partial cf-extract-entity-field
                                "service_plan_guid"))))))
