(ns cf-lib.util)

;; non-essential utils
(defn proxy-to-map
  "make sure proxy is a {:host host :port port} map
  and not a string like http://my-proxy:8080"
  [proxy]
  (when (and proxy (-> proxy empty? not))
    (condp instance? proxy
      java.util.Map proxy
      String (let [[all protocol host port]
                   (re-matches #"^(https?://)?(.*):([0-9]+)$"
                               proxy)]
               {:host host
                :port (Integer/parseInt port)}))))
