(ns me.arrdem.macros)

(defn- symbols
  "Amaloy's hack to find all used symbols in an arbitrary code/map/seq 
  framgment. Supports destructuring forms and other fancy stuff."  
  [coll]
  (filter symbol? (tree-seq coll? seq coll)))

(defmacro -<n>
  "A destructuring nested closure macro which defines <> to be the return value
   of the previous expression starting with an initial value computed by the 
   'expr' argument. Creates destructuring bindings so that <1> is the 1st seq
   value of <>, <2> the 2nd and soforth. This is designed to support processing
   models where a programmer wishes to manipulate a state map and some other 
   values simultaneously, propigating changes to the state map."
  ([expr form]
     (let [ns (->> (symbols form)
                   (map (fn [sym]
                         (when (symbol? sym)
                           (let [[_ v] (re-find #"<([0-9]+)>" (name sym))]
                             (when v (Integer. v))))))
                  (remove nil?))
           getters (reduce concat (map (juxt #(symbol (format "<%s>" %1))
                                             (fn [n] `(get ~'<n> ~n)))
                                       ns))]
       `(let [~'<> ~expr
              ~@getters]
          ~form)))

  ([expr f & forms]
     `(-<n> (-<n> ~expr ~f)
            ~@forms)))
