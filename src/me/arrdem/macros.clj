(ns ^{:doc "Provides various heavily used utility macros"
      :added "0.3.0"
      :author "Reid McKenize"}
      me.arrdem.macros)

(defn- symbols
  "amalloy's little toy for finding all symbols used in an expression."
  [coll]
  (filter symbol? (tree-seq coll? seq coll)))

(defmacro -<n>
  "A sequential evaluation & threading macro not unlike ->. -<n> evaluates
the first argument, binding it to the symbol <n>. It then creates bindings
of symbols named <[0-9]+>, so <1> or <2> and soforth _as they are used_ by
the programmer. So if <n> is an infinite length sequence one could reference
the 5th element via the symbol <5>. However this macro further allows for the
sequential evaluation of expressions rebinding all the carrot symbols and the
<n> value to the return value of the previous expression."
  ([expr form]
       `(let [~'<n> ~expr
              ~@(->> (symbols form)
                     (map (fn [sym]
                            (when (symbol? sym)
                              (let [[_ v] (re-find #"<([0-9]+)>" (name sym))]
                                (when v (Integer. v))))))
                     (remove nil?)
                     (map (juxt #(symbol (format "<%s>" %1))
                                (fn [n] `(get ~'<n> ~n))))
                     (reduce concat))]
          ~form))

  ([expr f & forms]
     `(-<n> (-<n> ~expr ~f)
            ~@forms)))
