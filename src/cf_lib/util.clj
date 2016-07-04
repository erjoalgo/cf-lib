(ns cf-lib.util)

;; non-essential utils
(defn proxy-to-map
  "make sure proxy is a {:host host :port port} map
  and not a string like http://my-proxy:8080"
  [proxy]
  (when proxy
    (condp instance? proxy
      java.util.Map proxy
      String (let [[all protocol host port]
                   (re-matches #"^(https?://)?(.*):([0-9]+)$"
                               proxy)]
               {:host host
                :port (Integer/parseInt port)}))))

(defmacro get-chain
  "(get-chain {1 {2 {3 {4 5}}}} 1 2 3) => {4 5}"
  [obj & accessors]
  ;;the same can be accomplished without a macro:
  ;;(reduce get {1 {2 {3 {4 5}}}} [1 2 3]) => {4 5}"
  (if (empty? accessors) obj
      `(get-chain (get ~obj ~(first accessors)) ~@(rest accessors))))

(defn current-time-secs []
  "return the current time in seconds"
  (-> (System/currentTimeMillis) (/ 1000) int))
