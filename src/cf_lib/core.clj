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

(defn cf-login! [cf-target]
  (let [username (get cf-target :user)
        password (get cf-target :pass)
        login-endpoint (clojure.string/replace (get cf-target :api-endpoint)
                                               #"api" "login")
        proxy-map (proxy-map (get cf-target :proxy))
        resp (client/post (str login-endpoint "/oauth/token")
                          {:basic-auth ["cf" ""]
                           :form-params {:username username
                                         :password password
                                         :grant_type "password"}
                           :accept :json
                           :proxy-host (get proxy-map :host)
                           :proxy-port (get proxy-map :port)
                           :insecure? true})
        body-json (json/read-str (get resp :body))
        oauth-token (get body-json "access_token")]
    (println (format "token obtained: %s"  oauth-token))
    (reset! (get cf-target :oauth-token) oauth-token)
    oauth-token))

(defn cf-curl [cf-target path
               & {:keys [method  http-client-args retry-count]
                  :or {method :get}}]
  (log/tracef "cf curl is %s %s\n" (name method) path)
  (when-not @(:oauth-token cf-target)
          (cf-login! cf-target))
  (assert @(get cf-target :oauth-token))
  (let [method-fun (case method
                     :get client/get
                     :post client/post
                     :delete client/delete)

        proxy-map (proxy-map (get cf-target :proxy))
        oauth-token @(get cf-target :oauth-token)
        additional-args {:headers {"Authorization" (str "bearer " oauth-token)}
                         :proxy-host (get proxy-map :host)
                         :proxy-port (get proxy-map :port)
                         :insecure? true}
        url (str (:api-endpoint cf-target) path)]
    (try
      (method-fun url (merge http-client-args additional-args))
      (catch clojure.lang.ExceptionInfo ex
        (clojure.stacktrace/print-stack-trace ex)
        (log/infof "token may have expired. status: %s, %s. ...\n"
                (-> ex .getData :status) (-> ex .getData :body))
        (if-not (and (-> ex .getData :status (= 401))
                     (or (not retry-count) (< retry-count 3)))
          ;; did not expire or max retries reached
          (throw ex)
          ;; retry
          (do (reset! (get cf-target :oauth-token) nil)
              (println "token was reset" )
              ;;TODO python **kwargs?
              (cf-curl cf-target path :method method
                       :http-client-args http-client-args
                       :retry-count (inc (or retry-count 0)))))))))


(defn cf-apps [cf-target]
  (let [resp (cf-curl cf-target :path "/v2/apps")]
    (get (json/read-str (get resp :body)) "resources")))

(defn cf-service [cf-target guid & {:keys [delete]}]
  ;/v2/services/b278c601-56d0-4a3b-a0b8-ca726a345b7d
  (let [method (if delete :delete :get)]
    (when delete
      (printf "deleteing service %s\n" guid))
    (cf-curl cf-target :path (str "/v2/service_instances/" guid)
             :method method)))

(defn app-name-from-json [app-json]
  (get-chain app-json "entity" "name"))

(defn cf-app-names [cf-target]
  (map app-name-from-json (cf-apps cf-target)))

(defn cf-app-guid [cf-target app-name]
  (let [apps (cf-apps cf-target)
        found (filter (comp (partial = app-name) app-name-from-json)
                      apps)]
    (when-not (empty? found)
            (get-chain (first found) "metadata" "guid"))))

(defn cf-app-routes [cf-target app-name]
  (let [guid (cf-app-guid cf-target app-name)
        ;routes-path (format "/v2/routes/%s/route_mappings" guid)
        routes-path (format "/v2/apps/%s/stats" guid)
        ;;routes-path (format "/v2/apps/%s/routes" guid)
        ;;/v2/apps/ad4cd7fb-5934-4e55-b012-56a331ff27ce/stats

        resp (cf-curl cf-target :path routes-path)
        body-json (json/read-str (get resp :body))

        uris (get-chain body-json "0" "stats" "uris" )
        ]
    uris))
