(ns cf-lib.core
  (:require
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [cf-lib.util :refer [proxy-to-map current-time-secs]]
   [cf-lib.token :refer [token-for-cf-target!]]
   [clojure.walk :refer [stringify-keys]]
   )
  (:gen-class))

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
   (map #(get % "resources"))
   (reduce concat)))

(defn cf-depaginate-resources
  "takes a paginated response, fetches all pages
  and accumulates their resources"
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

(defmacro cf-define-depaginating-functions
  "for each (fun-name url), define a cf function
 that takes a cf-target and optionally a guid, and make a cf-curl call"
  [& name-url-pairs]
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
           `(defn ~fun-name-sym
              ~(format "retrieve all %s" fun-name-sym)
              ~arg-list
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
 [cf-space-routes "/v2/spaces/%s/routes"]
 [cf-space-service-instances "/v2/spaces/%s/service_instances"]
 [cf-space-services "/v2/spaces/%s/services"]
 [cf-space-service-brokers "/v2/spaces/%s/service_brokers"]

 [cf-routes "/v2/routes"]
 [cf-route-apps "/v2/routes/%s/apps"]

 [cf-services "/v2/services"]
 [cf-service-plans "/v2/services/%s/service_plans"]
 [cf-service-plan-service-instances "/v2/service_plans/%s/service_instances"]
 [cf-service-brokers "/v2/service_brokers"]

 [cf-users "/v2/users"]
 [cf-orgs "/v2/organizations"]

 [cf-shared-domains "/v2/shared_domains"]
 [cf-private-domains "/v2/private_domains"]
 )

(defn format-sym
  "like format, but return a symbol instead of a string"
  [fmt & args]
  (->> args
       (apply format fmt)
       symbol))

(defmacro cf-define-get-delete-functions
  "for each (fun-name url) pair, define
1. a getter function that takes a cf-target and a guid,
  2. a deleter for the same resource"
  [& name-url-pairs]
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
                     `(defn ~cf-fun
                        ~(format "retrieve a particular %s by guid" cf-fun)
                        ~(apply vector
                                cf-target-get
                                guid-syms-get)
                        (let [url# (format ~url ~@guid-syms-get)]
                          (-> (cf-curl ~cf-target-get url#)
                              :body json/read-str)))

                     `(defn ~cf-fun-delete
                        ~(format "delete a particular %s by guid" cf-fun)
                        ~(apply vector
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
 [cf-service-broker "/v2/service_brokers/%s"]
 [cf-service-binding "/v2/service_bindings/%s"]
 [cf-service-instance "/v2/service_instances/%s"]
 [cf-service-plan "/v2/service_plans/%s"]
 [cf-service "/v2/services/%s"]
 [cf-route "/v2/routes/%s"]
 [cf-org "/v2/organizations/%s"]

 [cf-shared-domain "/v2/shared_domains/%s"]
 [cf-private-domain "/v2/private_domains/%s"]
 )

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

(defmacro cf-define-to-from-name-functions
  "for each cf-fun, define functions,
1. $(cf-fun)-by-name to lookup by name
2. $(cf-fun)-name, to extract the name (returns a string)
3. $(cf-fun)-name-to-guid, to extract the guid (returns a string)

cf-fun-sym must be an existing function"
  [& cf-fun-syms]
  `(do ~@(->> (map (fn [cf-fun-sym]
                     (let [find-by-name-sym (format-sym "%s-by-name" cf-fun-sym)
                           cf-fun-sym-plural (format-sym "%ss" cf-fun-sym)
                           to-name-sym (format-sym "%s-name" cf-fun-sym)
                           name-to-guid-sym (format-sym "%s-name-to-guid"
                                                        cf-fun-sym)
                           ]
                       [
                        `(defn ~to-name-sym
                           ~(format "extracts a %s's name" cf-fun-sym)
                           [cf-target# guid#]
                           (-> (~cf-fun-sym cf-target# guid#)
                               (cf-extract-name)))

                        `(defn ~find-by-name-sym
                           ~(format "lookup a %s by name" cf-fun-sym)
                           [cf-target# name#]
                           (let [matches#
                                 (->> (~cf-fun-sym-plural cf-target#)
                                      (filter (comp (partial = name#)
                                                    cf-extract-name)))
                                 ]
                             (when (-> (rest matches#) empty?)
                               (log/warnf
                                ~(format
                                  "multiple matches for %s with name: %%s"
                                  cf-fun-sym) name#))
                             (first matches#)))

                        `(defn ~name-to-guid-sym
                           ~(format "map a %s's name to its guid" cf-fun-sym)
                           [cf-target# name#]
                           (-> (~find-by-name-sym cf-target# name#)
                               cf-extract-guid))
                        ]
                       ))
                   cf-fun-syms)
              (reduce concat))))

(cf-define-to-from-name-functions
 cf-app
 cf-space
 cf-service-instance
 cf-org
 cf-shared-domain)

(defmacro cf-define-resource-all-delete
  "define a function to delete all resources.
 args is a list of (resource, deleter) pairs
 corresponding to existing cf functions.
resources should retrieve all resources.
deleter deletes a single resource
  "
  [& resource-deleter-pairs]
  `(do ~@(->> resource-deleter-pairs
        (map (fn [[resource deleter description]]
               (let [all-deleter-sym (format-sym "%s-delete" resource)]
                 `(defn ~all-deleter-sym
                    ~description
                    [cf-target# guid#]
                    (->> (~resource cf-target# guid#)
                         (pmap (comp (partial ~deleter cf-target#)
                                    cf-extract-guid))
                         doall))
                 )))
        )))

(cf-define-resource-all-delete
 [cf-app-bindings cf-app-binding-delete
  "delete all bindings for an app" ]
 [cf-service-instance-bindings cf-service-binding-delete
  "delete all bindings for a service instance"]
 [cf-app-routes cf-route-delete
  "delete all routes for an app"])

(defn cf-app-delete-force
  "delete an app by force: unbinds all service instances and
  deletes any associated routes"
  [cf-target app-guid]
  ;; need to remember app routes before deletion.
  (let [app-route-guids (->> (cf-app-routes cf-target app-guid)
                             (map cf-extract-guid)
                             doall)]
    (hash-map
     :binding-resps (cf-app-bindings-delete cf-target app-guid)
     :app-resp (cf-app-delete cf-target app-guid)
     :route-resps (-> (pmap (partial cf-route-delete cf-target)
                            app-route-guids)
                      doall))))


(defn cf-service-instance-delete-force
  "delete a service instance by force: unbinds from all bound apps"
  [cf-target service-instance-guid]
  (do (cf-service-instance-bindings-delete cf-target service-instance-guid)
      (cf-service-instance-delete cf-target service-instance-guid)))

(defn cf-service-instance-create
  "create a service instance"
  [cf-target name service-plan-guid space-guid
   & {:keys [payload]}]
  (cf-curl cf-target "/v2/service_instances" :verb :POST
           :body {"name" name
                  "service_plan_guid" service-plan-guid
                  "space_guid" space-guid
                  "parameters" payload}))

(defn cf-service-by-label
  "retrieve a service by its label"
  [cf-target service-label]
  (->> (cf-services cf-target)
      (filter (comp (partial = service-label)
                    (partial cf-extract-entity-field "label")))
      first))

;;redefine this function: it makes no sense to search across all services
;;for a plan by given name, ie a lot of duplicates, "Free", "Tiered", etc
;;this function was never actually defined
(defn cf-service-plan-by-name
  "retrieve a service plan by its service guid and name"
  [cf-target service-guid service-plan-name]
  (->> (cf-service-plans cf-target service-guid)
       (filter (comp (partial = service-plan-name)
                     cf-extract-name))
       first))

(defn cf-service-instance-create-human
  "human-friendly create service, similar to cf create-service"
  [cf-target name
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
     name service-plan-guid space-guid :payload payload)))

(defn cf-get-envs
  "retrieve all envs for an app"
  [cf-target app-guid]
  (-> (cf-curl cf-target
               (format "/v2/apps/%s/env" app-guid))
      :body json/read-str))

(defn cf-set-envs
  "update existing app envs map with envs"
  [cf-target app-guid envs]
  (let [current-envs (cf-get-envs cf-target app-guid)]
    (cf-curl
     cf-target
     (format "/v2/apps/%s" app-guid)
     :verb :PUT
     :body {:environment_json (merge current-envs envs)})))

(defn cf-app-vcap-services
  "extract VCAP_SERVICES for an app"
  [cf-target app-guid]
  (-> (cf-get-envs cf-target app-guid)
      (get "system_env_json")))


(defn cf-service-instances-by-service-label
  "retrieve all service instances for a service, specified by its label"
  [cf-target service-label]
  (let [service-guid (-> (cf-service-by-label cf-target service-label)
                         cf-extract-guid)
        service-plan-guids (->> (cf-service-plans cf-target service-guid)
                                (map cf-extract-guid))
        ]
    (->> service-plan-guids
         (map (partial cf-service-plan-service-instances cf-target))
         (apply concat))))

(defn cf-route-url
  "construct a ROUTE.DOMAIN[/PATH] string for a route id,
  returning a url sans protocol"
  [cf-target route-id]
  (let [route-payload (cf-route cf-target route-id)
        host (cf-extract-entity-field "host" route-payload)

        ;;on empty, will be empty str, not null
        path (cf-extract-entity-field "path" route-payload)

        ;;domain-guid (cf-extract-entity-field "domain_guid" route-payload)
        ;;we don't know whether shared or non-shared domain, just use domain_url
        domain-url (cf-extract-entity-field "domain_url" route-payload)
        domain-payload (-> (cf-curl cf-target domain-url)
                           :body json/read-str)
        domain-name (cf-extract-name domain-payload)
        ]
    (format "%s.%s%s" host domain-name path)))

(defn cf-route-create
  "create a new route"
  [cf-target domain-guid space-guid {:keys [host port path] :as extra}]
  (cf-curl cf-target "/v2/routes" :verb :POST
           :body (-> {"domain_guid" domain-guid
                      "space_guid" space-guid}
                     (merge (->> (filter second extra)
                                 flatten (apply hash-map)
                                 )))))

(defn cf-map-route
  "map a route to an app"
  [cf-target route-guid app-guid]
  (cf-curl cf-target "/v2/route_mappings" :verb :POST
           :body (-> {"app_guid" app-guid
                      "route_guid" route-guid})))

(defn cf-app-restage "restage an app"
  [cf-target app-guid]
  ;;POST /v2/apps/63e33a8c-7bc0-498e-b132-ca9dba4dbdab/restage
  (cf-curl cf-target (format "/v2/apps/%s/restage" app-guid)
           :verb :POST))
