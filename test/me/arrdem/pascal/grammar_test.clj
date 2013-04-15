(ns me.arrdem.pascal.grammar-test
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.symtab :refer [search]]
            [me.arrdem.pascal :refer [process-string build-ast]]
            [me.arrdem.pascal.symtab :refer [init! clear!]]
            [me.arrdem.pascal.lexer :refer [pascal]]
            [name.choi.joshua.fnparse :as fnp]))

(deftest pointer-def-case
  (binding [me.arrdem.compiler.symtab/*symtab* (atom {})]
    (init!)
    (let [res (-> (fnp/rule-match
                   me.arrdem.pascal.grammar/variable-declaration
                   #(println "FAILED: " %)
                   #(println "LEFTOVER: " %2)
                   {:remainder (pascal "var i, j, k : ^ integer;
                                       l, m, n : ^ foo")})
                  rest rest)]
      (println res)
      (doseq [i res]
        (println i)
        (is (= i
               (:qname (search i))))))))
