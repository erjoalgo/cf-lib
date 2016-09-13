(ns cf-lib.token
  (:require
   [clj-http.client :as client]
   [clojure.data.json :as json]
   [clojure.tools.logging :as log]
   [cf-lib.util :refer [proxy-to-map]]
   ))

(def min-token-refresh-secs 5)

(defn- current-time-secs []
  "return the current time in seconds"
  (-> (System/currentTimeMillis) (/ 1000) int))

(defn cf-token [cf-target]
  "obtain a token given either (username password) or authorization-code"
  (log/infof "obtaining token for target: %s"  cf-target)
  (let [username (:user cf-target)
        password (:pass cf-target)
        auth-code-login (:auth-code-login cf-target)
        login-endpoint (or (:uaa-endpoint cf-target)
                           (clojure.string/replace
                            (:api-endpoint cf-target)
                            #"api" "login"))
        token-url (str login-endpoint "/oauth/token")
        proxy-map (proxy-to-map (:proxy cf-target))
        req-params-common {:accept :json
                           :proxy-host (:host proxy-map)
                           :proxy-port (:port proxy-map)
                           :insecure? (:insecure? cf-target)}
        req-params-extra (cond (and username password)
                               {:basic-auth ["cf" ""]
                                :form-params {:username username
                                              :password password
                                              :grant_type "password"}}
                               auth-code-login
                               (let [{:keys
                                      [auth-code client-id client-secret
                                       scopes redirect-uri]} auth-code-login]

                                 {:query-params
                                  {"grant_type" "authorization_code"
                                   "client_id" client-id
                                   "client_secret" client-secret
                                   "response_type" "token"
                                   ;"token_format" "opaque"
                                   "code" auth-code
                                   "scope" (clojure.string/join " " scopes)
                                   "redirect_uri" redirect-uri}
                                  :content-type
                                  "application/x-www-form-urlencoded"
                                  })
                               true (throw (Exception.
                                            "must provide user/pass or code")))
        resp (client/post token-url (merge req-params-common req-params-extra))
        oauth-token (-> resp :body json/read-str
                        (get "access_token"))]
    (log/infof "token obtained: %s"  oauth-token)
    (printf "token obtained: %s\n"  oauth-token)
    oauth-token))

(def cf-target-to-token
  "map cf targets to tokens accros threads"
  (atom {}))

(defn- assoc-if [pred map key val]
  "like assoc, but uses a predicate on the (possibily nil)
existing association value to decide whether to assoc"
  (if (pred (get map key))
    (assoc map key val)
    map))

(defn- token-too-recent? [[ctime-secs token]]
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

