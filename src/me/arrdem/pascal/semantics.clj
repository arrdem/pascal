(ns me.arrdem.pascal.semantics
  (:require [clojure.pprint :refer [pprint]]
            [me.arrdem.pascal.symtab :refer [genlabel! install! search]]
            [name.choi.joshua.fnparse :as fnp]))

;;------------------------------------------------------------------------------
;; OLD generation code..
(defn binop [args]
  (let [[e0 op e1] args]
    `(~op ~e0 ~e1)))

(defn makeif [[_0 test _1 s elsepart]]
  (let [else (if elsepart (second elsepart))]
    `(~'if ~test ~s ~else)))

(defn makewhile [[_0 test _1 s]]
  (let [label (genlabel!)]
    `((~'label ~label)
      (~'if ~test
          (~'do ~s
              (~'goto ~label))))))

(defn makerepeat [[_0 s _1 test]]
  (let [label (genlabel!)]
    `((~'label ~label)
      ~s
      (~'if (~'not ~test)
            (~'goto ~label)))))

(defn makederef [sym]
  `(~'deref ~(symbol (:qname (search sym)))))

(defn makefuncall [[sym _ args]]
  `(funcall ~(symbol sym) ~@args))


;;------------------------------------------------------------------------------
;; NEW generation code..

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
  `(~'comment "defined variables" ~@(reduce concat decls)))

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
  `(~'program ~id ~@heading ~@block))

(defn program-heading
  [[_l ids _r]]
  (map (fn [i] `(~'progn ~i))
       ids))

(defn block
  [[comments progn]]
  (concat comments (list progn)))

(defn tail-cons
  [[s [_ rest]]]
  (cons s rest))

(defn block2progn
  [[_0 exprs _1]]
  `(~'progn ~@exprs))

(defn for-downto
  [[s0 _ sf]]
  [s0 `(~'- 1) `(~'>= ~sf)])

(defn for-to
  [[s0 _ sf]]
  [s0 `(~'+ 1) `(~'<= ~sf)])

(defn for-stmnt [[_0 id _1 flist _3 stmnts]]
  (let [[Vi update end] flist
        lstart (genlabel!)
        lend   (genlabel!)]
    `(~'progn
      (~'label ~lstart)
      (~':= ~id ~Vi)
      (~'if (~@end ~id)
        (~'goto ~lend))
      ~stmnts
      (~':=  ~id (~@update ~id))
      (~'goto ~lstart)
      (~'label ~lend))))

(defn procinvoke
  [[id [_0 params _1]]]
  `(~'funcall ~id ~@params))
