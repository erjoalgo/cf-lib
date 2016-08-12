(ns cf-lib.token
  (:require
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [cf-lib.util :refer [proxy-to-map current-time-secs]]
   ))

(def min-token-refresh-secs 5)

(defn cf-token [cf-target]
  "obtain a token"
  (log/infof "obtaining token for target: %s"  cf-target)
  (let [username (:user cf-target)
        password (:pass cf-target)
        login-endpoint (or (:uaa-endpoint cf-target)
                           (clojure.string/replace
                            (:api-endpoint cf-target)
                            #"api" "login"))
        proxy-map (proxy-to-map (:proxy cf-target))
        resp (client/post (str login-endpoint "/oauth/token")
                          {:basic-auth ["cf" ""]
                           :form-params {:username username
                                         :password password
                                         :grant_type "password"}
                           :accept :json
                           :proxy-host (:host proxy-map)
                           :proxy-port (:port proxy-map)
                           :insecure? (:insecure? cf-target)})
        oauth-token (-> resp :body json/read-str
                        (get "access_token"))]
    (log/infof "token obtained: %s"  oauth-token)
    (printf "token obtained: %s"  oauth-token)
    oauth-token))

(def cf-target-to-token
  "map cf targets to tokens accros threads"
  (atom {}))

(defn assoc-if [pred map key val]
  "like assoc, but uses a predicate on the (possibily nil)
existing association value to decide whether to assoc"
  (if (pred (get map key))
    (assoc map key val)
    map))

(defn token-too-recent? [[ctime-secs token]]
  "returns true if a token is too recent to refresh"
  (when ctime-secs
    (let [elapsed-time (- (current-time-secs) ctime-secs)]
      (< elapsed-time min-token-refresh-secs))))

(defn token-for-cf-target! [cf-target & {:keys [force]}]
  "returns a token for cf-target. if no token exists or if
:force is true obtain a new token and associate it with target.
value is cached accross threads, and a token is refreshed at most once
within min-refresh-delay"
  (let [existing (and (not force)
                      (get @cf-target-to-token cf-target))
        secs-token (or existing
                          (-> (swap! cf-target-to-token
                                     (partial assoc-if
                                              (comp not token-too-recent?))
                                     cf-target
                                     [(current-time-secs)
                                      (delay (cf-token cf-target))])
                              (get cf-target)))]
    (-> secs-token second deref)))

