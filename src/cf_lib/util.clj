(ns cf-lib.util)

;; non-essential utils
(defn proxy-to-map [proxy]
  "make sure proxy is a {:host host :port port} map
and not a string like http://my-proxy:8080"
  (when proxy
    (condp instance? proxy
      java.util.Map proxy
      String (let [[_ host port] (re-matches #"^(.*):([0-9]+)$" proxy)]
               {:host host
                :port port}))))

(defmacro get-chain [obj & accessors]
  "(get-chain {1 {2 {3 {4 5}}}} 1 2 3) => {4 5}"
  ;;the same can be accomplished without a macro:
  ;;(reduce get {1 {2 {3 {4 5}}}} [1 2 3]) => {4 5}"
  (if (empty? accessors) obj
      `(get-chain (get ~obj ~(first accessors)) ~@(rest accessors))))
