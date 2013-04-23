(ns me.arrdem.pascal.core-test
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.symtab :refer [search]]
            [me.arrdem.compiler :refer [nameof typeof sizeof]]
            [me.arrdem.pascal.test-text :as data]
            [me.arrdem.pascal :refer [process-string build-ast]]
            [me.arrdem.pascal.symtab :refer [with-symtab]]))

(defmacro full-test-case [sym val]
  `(deftest ~sym
     (testing
         (with-symtab
           (let [result# (process-string (:text ~val))]
             ;; check AST result...
             (is (= result# (:ast ~val)))
             ;; check symbol table contents...
             (doseq [s# (:symbols ~val)]
               (let [entered-sym# (search (nameof s#))]

                 (is (= (nameof entered-sym#)
                        (nameof s#))
                     "No such symbol table entry")

                 (is (= (nameof (typeof entered-sym#))
                        (nameof (typeof s#)))
                     "The entered symbol did not match types")

                 (is (= (sizeof entered-sym#)
                        (sizeof s#))
                     "Entered size doesn't match"))))))))

;;------------------------------------------------------------------------------
;; the big test cases over assignment inputs...

(full-test-case triv-pas-test data/triv-pas)    ;; parser assignment 1
(full-test-case trivb-pas-test data/trivb-pas)  ;; parser assignment 1
(full-test-case graph1-pas-test data/graph1-pas) ;; parser assignment 2
