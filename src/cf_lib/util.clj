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
