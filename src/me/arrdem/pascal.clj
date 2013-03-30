(ns me.arrdem.pascal
  (:require [clojure.pprint :refer :all]
            [me.arrdem.pascal.grammar :refer [pascal-program]]
            [me.arrdem.pascal.lexer :refer [pascal]]
            [me.arrdem.pascal.symtab :refer [*symtab* render-ns]]
            [name.choi.joshua.fnparse :as fnp])
  (:gen-class :main true))

(def code-pp
  "A wrapper around pprint which makes its code-formatting setting more
easily accessed."
  #(with-pprint-dispatch code-dispatch (pprint %)))

(defn build-ast [toks]
  (fnp/rule-match
   pascal-program
   #(println "FAILED: " %)
   #(println "LEFTOVER: " %2)
   {:remainder toks}))

(def process-string
  (comp build-ast pascal))

(def fmnt "%-25s : %s")

(defn -main
  "The only valid arguments are targeted files. If there are no targeted files
then decomp will target stdin as its token source."
  [& args]
  (if-not (empty? args)
    (doseq [f args]
      (println "attempting to read file" f)
      (-> f
          slurp
          process-string
          code-pp)
      (println (apply str (repeat 80 \-)))
      (doseq [[k v] @*symtab*]
        (println (format fmnt (str k) (str v))))
      (reset! *symtab* me.arrdem.pascal.symtab/base_st)
      nil)

    (do (-> *in*
            slurp
            process-string
            code-pp)
        (println (apply str (repeat 80 \-)))
        (doseq [[k v] @*symtab*]
          (if (list? k)
            (println (format fmnt (render-ns k) (str v)))))
        (reset! *symtab* me.arrdem.pascal.symtab/base_st)
        nil)))
