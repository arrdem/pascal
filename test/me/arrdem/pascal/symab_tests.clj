(ns me.arrdem.pascal.core-test
  (:use clojure.test
        me.arrdem.pascal.symtab))

(defmacro symtab-wrapper [& forms]
  `(binding [me.arrdem.pascal.symtab/*symns* (atom '("toplevel"))
             me.arrdem.pascal.symtab/*symtab* (atom {})]
     ~@forms))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
