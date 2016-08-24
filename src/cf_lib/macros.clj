(in-ns 'cf-lib.core)

(defn- format-sym
  "like format, but return a symbol instead of a string"
  [fmt & args]
  (->> args
       (apply format fmt)
       symbol))

(defmacro cf-define-depaginating-functions
  "for each (fun-name url), define a cf function
  that takes a cf-target and optionally a guid, and make a cf-curl call"
  [& name-url-pairs]
  `(do ~@(map (fn [[fun-name-sym url]]
                (let [needs-format (.contains url "%s")
                      cf-target-sym (gensym "cf-target-")
                      q-params-sym (gensym "q-params")
                      guid-sym (gensym "guid-")
                      arg-list `[~cf-target-sym
                                 ~@(when needs-format [guid-sym])
                                 & {~q-params-sym :q}]
                      url-form (if needs-format
                                 `(format ~url ~guid-sym)
                                 url)
                      ]
                  `(defn ~fun-name-sym
                     ~(format "retrieve all %s" fun-name-sym)
                     ~arg-list
                     (->> (cf-curl ~cf-target-sym ~url-form
                                   :query-params
                                   {:q (map (partial clojure.string/join ":")
                                            ~q-params-sym)})
                          (cf-pdepaginate-resources ~cf-target-sym)))
                  ))
              name-url-pairs)))

(defmacro cf-define-get-delete-functions
  "for each (fun-name url) pair, define
  1. a getter function that takes a cf-target and a guid,
  2. a deleter for the same resource"
  [& name-url-pairs]
  `(do
     ~@(->> (mapcat (fn [[cf-fun url]]
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
            do)))

(defmacro cf-define-to-from-name-functions
  "for each cf-fun, define functions,
1. $(cf-fun)-by-name to lookup by name
2. $(cf-fun)-name, to extract the name (returns a string)
3. $(cf-fun)-name-to-guid, to extract the guid (returns a string)

cf-fun-sym must be an existing function"
  [& cf-fun-syms]
  `(do ~@(->> (mapcat (fn [cf-fun-sym]
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
                           (let [matches# (~cf-fun-sym-plural cf-target#
                                           :q {"name" name#})]
                             (when-not (-> (rest matches#) empty?)
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
                   cf-fun-syms))))

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
