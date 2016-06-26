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

(defstruct cf-target
  :api-endpoint :user :pass :proxy :oauth-token)
;;TODO fill in nil atom on the struct

(defn proxy-map [proxy]
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
  (ref {}))

(defn token-for-cf-target! [cf-target & {:keys [force]}]
  "returns a token for cf-target. if no token exists or if
:force is true obtain a new token and associate it with target"
  (let [existing (and (not force)
                      (get cf-target-to-token cf-target))]
    ;;TODO ensure anyone reading ref is BLOCKED until token is obtained?
    (or existing (dosync (ref-set cf-target-to-token
                                  (cf-token cf-target))))))
(defn cf-curl [cf-target path
               & {:keys [verb http-client-args retry-count]
                                 :or {verb :get
                                      retry-count 0}}]
  (log/tracef "cf curl is %s %s\n" (name verb) path)
  (let [verb-fun (case verb
                   :get client/get
                   :post client/post
                   :delete client/delete)

        url (str (:api-endpoint cf-target) path)

        proxy-map (proxy-map (:proxy cf-target))
        proxy-args {:proxy-host (:host proxy-map)
                    :proxy-port (:port proxy-map)
                    :insecure? (:insecure? cf-target)}

        token-header {"Authorization"
                      (->> (token-for-cf-target! cf-target
                                                 :force (> retry-count 0))
                           (str "bearer "))}
        headers {:headers (merge (:headers http-client-args)
                                token-header)}
        ]
    (try (verb-fun url
                   (merge http-client-args proxy-args headers))
         (catch Exception ex
           ;(clojure.stacktrace/print-stack-trace ex)
           (if-not
               (and (< retry-count 2)
                    ;;TODO check if has .getData method
                    (-> ex .getData :status (= 401)))
             (throw ex)
             (cf-curl cf-target path
                      :verb verb
                      :http-client-args http-client-args
                      :retry-count (inc (or retry-count 0))))))))

(defn cf-depaginate-resources [cf-target resp]
  (->>
   resp
   (iterate #(let [next-url
                   (-> %
                       :body
                       json/read-str
                       (get  "next_url"))]
               (when next-url
                 (cf-curl cf-target next-url))))
   (take-while (comp not nil?))
   (map (comp #(get % "resources") json/read-str :body))
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
                   (cf-depaginate-resources ~cf-target-sym)))
           ))
       name-url-pairs)))

(cf-define-depaginating-functions
 [cf-apps "/v2/apps"]
 [cf-app-routes "/v2/apps/%s/routes"]
 [cf-app-bindings "/v2/apps/%s/service_bindings"]

 [cf-service-instances "/v2/service_instances"]
 [cf-service-instance-bindings "/v2/service_instances/%s/service_bindings"]

 [cf-spaces "/v2/spaces"]
 [cf-space-apps "/v2/spaces/%s/apps"];TODO fix others using name instead of guid
 [cf-space-service-instances "/v2/spaces/%s/service_instances"]

 [cf-services "/v2/spaces/%s/service_instances"]

 [cf-users "/v2/users"]
 [cf-orgs "/v2/orgs"]
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
                                       :verb :delete))))
                     ]
                    ))
                name-url-pairs)
           (reduce concat)
           do)))



(cf-define-get-delete-functions
 [cf-app "/v2/apps/%s"]
 [cf-space "/v2/spaces/%s"]
 [cf-app-binding "/v2/apps/%s/service_bindings/%s"]
 [cf-service-instance-binding "/v2/apps/%s/service_bindings/%s"]
 [cf-service-instance "/v2/service_instances/%s"]
 [cf-service-plan "/v2/service_plans/%s"]
 [cf-service "/v2/services/%s"]
 [cf-route "/v2/routes/%s"]
 )

(defn cf-extract-name [resp]
  (reduce get resp ["entity" "name"]))

(defn cf-extract-guid [resp]
  (reduce get resp ["metadata" "guid"]))

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

(defn cf-app-bindings-delete [cf-target app-guid]
  "delete all bindings for an app"
  (->>
   (cf-app-bindings cf-target app-guid)
   (map cf-extract-guid)
   (map (partial cf-app-binding-delete cf-target app-guid))
   dorun))

(defn cf-service-instance-bindings-delete [cf-target service-instance-guid]
  "delete all bindings for a service"
  (->> (cf-service-instance-bindings cf-target service-instance-guid)
       (map cf-extract-guid)
       (map #(cf-service-instance-binding-delete
              cf-target
              %;;app name first
              service-instance-guid))
       dorun))

(defn cf-app-routes-delete [cf-target app-guid]
  (->> (cf-app-routes cf-target app-guid)
       (map cf-extract-guid)
       (map (partial cf-route-delete cf-target))
       dorun))

(defn cf-app-delete-force [cf-target app-guid]
  (do
    (cf-app-bindings-delete cf-target app-guid)
    (cf-app-delete cf-target app-guid)
    (cf-app-routes-delete cf-target app-guid)))

(defn cf-service-instance-delete-force [cf-target service-instance-guid]
  (do (cf-service-instance-bindings-delete cf-target service-instance-guid)
      (cf-service-instance-delete cf-target service-instance-guid)))

(defn cf-service-plan-guid-to-service-label [cf-target guid]
  (->> (cf-service-plan cf-target guid)
       (#(reduce get % ["entity" "service_guid"]))
       (cf-service cf-target)
       (#(reduce get % ["entity" "label"]))))

