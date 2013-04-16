(ns me.arrdem.pascal.grammar-test
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.symtab :refer [search install!]]
            [me.arrdem.compiler.symbols :refer [nameof typeof]]
            [me.arrdem.pascal :refer [process-string build-ast]]
            [me.arrdem.pascal.symtab :refer [init! clear!]]
            [me.arrdem.pascal.lexer :refer [pascal]]
            [name.choi.joshua.fnparse :as fnp]))

(deftest pointer-def-case
  (binding [me.arrdem.compiler.symtab/*symtab* (atom {})]
    (clear!)
    (install! {:name "^foo" :type "integer"})
    (let [res (-> (fnp/rule-match
                   me.arrdem.pascal.grammar/variable-declaration
                   #(println "FAILED: " %)
                   #(println "LEFTOVER: " %2)
                   {:remainder (pascal "var i, j, k : ^ integer;
                                       l, m, n : ^ foo")})
                  rest rest)]
      (doseq [i res]
        (let [r (search i)]

          (is (= i
                 (nameof r))
              "Is the installed really installed?"))))))
