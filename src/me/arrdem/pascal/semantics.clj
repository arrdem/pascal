(ns me.arrdem.pascal.semantics
  (:require [clojure.pprint :refer [pprint]]
            [me.arrdem.pascal.symtab :refer [genlabel! install!]]
            [name.choi.joshua.fnparse :as fnp]))

(defn variableid-list
  ([[first [_ rest]]]
     (cons first rest)))

(defn vardecl
  "Semantics for the vardecl rule in grammar.clj.
   Enters vars with their types in the symbol table."
  [[varseq _ type]]
  (doseq [v varseq]
    (let [v {:name v
             :type :symbol
             :type/data type}]
      (pprint v)
      (install! v)))
  varseq)

(defn const-assign
  [[id _ v]]
  (let [v {:name      id
           :value     (:value v)
           :type      (:type v)}]
    (pprint v)
    (install! v)))

(defn string
  [s]
  {:value s
   :type  "string"})
