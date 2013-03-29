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

(defn vardecls
  [[decl [_ rest]]]
  (cons decl rest))

(defn variable-declaration
  [[_ decls]]
  decls)

(defn const-assign
  [[id _ v]]
  (let [v {:name      id
           :value     (:value v)
           :type      (:type v)}]
    (install! v)
    v))

(defn constant-declaration
  [[_ c0 cs]]
  (let [cs (map second cs)]
    `(~'comment "got constant decl group:"
                ~@(cons (:name c0) (map :name cs)))))

(defn string
  [s]
  {:value s
   :type  "string"})

(defn integer
  [[sign? ival]]
  (let [factor (case sign?
                 (+ nil) 1
                 (-) -1)]
    {:value (* factor ival)
     :type  "integer"}))

(defn real
  [[sign? rval]]
  (let [factor (case sign?
                 (+ nil) 1
                 (-) -1)]
    {:value (* factor rval)
     :type  "real"}))

(defn label-declaration
  [[_l l0 ls]]
  (println l0)
  (println ls)
  (let [ls (map second ls)]
    `(~'comment "found label declarations" ~@(cons l0 ls))))

(defn variable
  [[id postfixes]]
  (if (empty? postfixes)
    id
    `(~'-> ~id
           ~@postfixes)))

(defn additive-expression
  [[me tail]]
  (if-let [[op adxpr] tail]
    `(~op ~me ~adxpr)
    me))

(defn assignment
  [[target assignop expr]]
  `(~assignop ~target ~expr))

(defn pascal-program
  [[progn id heading _ block __]]
  `(~'program ~id ~@heading ~block))

(defn program-heading
  [[_l ids _r]]
  (map (fn [i] `(~'progn ~i))
       ids))

(defn identifier-list
  [[id [_ rest]]]
  (cons id rest))
