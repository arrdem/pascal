(ns me.arrdem.pascal.semantics
  (:require [clojure.pprint :refer [pprint]]
            [me.arrdem.compiler.symtab :refer [genlabel! install!
                                               search gensym! render-ns]]
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
;; TODO: reduce this out, need to eliminate calls to the other three from the
;;       grammar before I can delete them here.

(defn tail-cons
  [[s [_ rest]]]
  (cons s rest))

(def cons-ht tail-cons)
(def variableid-list tail-cons)
(def vardecls tail-cons)

;;------------------------------------------------------------------------------
;; some supporting functions...

(defn abs-name
  [sym]
  (or (:qname sym)
      (:qname (search sym))))

(defn dbg-install
  ([v]
     (install! v)
     (:name v)))

;;------------------------------------------------------------------------------
;; NEW generation code..

(defn vardecl
  "Semantics for the vardecl rule in grammar.clj.
   Enters vars with their types in the symbol table."
  [[varseq _ type]]
  (doseq [v varseq]
    (let [v {:name v
             :type :symbol
             :type/data type}]
      (dbg-install v)))
  (map (comp abs-name search)
       varseq))

(defn variable-declaration
  [[_ decls]]
  `(~'comment "defined variables" ~@(reduce concat decls)))

(defn const-assign
  [[id _ v]]
  (let [v {:name      id
           :value     v
           :type      :symbol
           :type/data :reference}]
    (dbg-install v)))

(defn constant-declaration
  [[_ c0 cs]]
  (let [cs (map second cs)]
    `(~'comment "got constant decl group:"
                ~@(cons c0 cs))))

(defn string
  [s]
  (let [sym (gensym! "str_")
        val {:name sym
             :value s
             :type :symbol
             :type/data "string"}]
    (dbg-install val)
    (abs-name (search sym))))

(defn snum
  [prefix [sign? rval]]
  (let [factor (case sign?
                 (+ nil) 1
                 (-) -1)
        sym (gensym! (str prefix "_"))
        val {:name sym
             :value (* factor rval)
             :type :symbol
             :type/data prefix}]
    (dbg-install val)))

(def integer (partial snum "integer"))
(def real (partial snum "real"))

(defn label-declaration
  [[_l l0 ls]]
  (let [ls (map second ls)]
    `(~'comment "found label declarations" ~@(cons l0 ls))))

(defn variable
  [[id postfixes]]
  (let [id (abs-name (search id))]
    (assert id)
    (if (empty? postfixes)
      id
      `(~'-> ~id
             ~@postfixes))))

(defn additive-expression
  [[me tail]]
  (if-let [[op adxpr] tail]
    `(~op ~me ~adxpr)
    me))

(defn assignment
  [[target assignop expr]]
  `(~assignop ~target ~expr))

(defn pascal-program
  [[[_0 id] heading _1 block _2]]
  `(~'program ~id ~@heading ~@block))

(defn program-heading
  [[_l ids _r]]
  (map (fn [i] `(~'progn ~i))
       ids))

(defn block
  [[comments progn]]
  (concat comments (list progn)))

(defn block2progn
  [[_0 exprs _1]]
  `(~'progn ~@(remove nil? exprs)))

(defn for-downto
  [[s0 _ sf]]
  [s0 `(~'- 1) '>= sf])

(defn for-to
  [[s0 _ sf]]
  [s0 `(~'+ 1) '<= sf])

(defn for-stmnt [[_0 id _1 flist _3 stmnts]]
  (let [[Vi update comp end] flist
        lstart (genlabel!)
        id     (abs-name (search id))]
    `(~'progn
      (~'label ~lstart)
      (~':= ~id ~Vi)
      (~'if (~comp ~id ~end)
        (~'progn ~@stmnts
                 (~':=  ~id (~@update ~id))
                 (~'goto ~lstart))))))

(defn repeat-stmnt
  [[_rep stmnts _unt expr]]
  (let [lbl (genlabel!)]
    `(~'progn (~'label ~lbl)
            ~@stmnts
            (~'if (~'not ~expr)
              (~'goto ~lbl)))
    ))

(defn procinvoke
  [[id [_0 params _1]]]
  `(~'funcall ~id ~@params))

(defn identifier
  ([id] (abs-name (search id))))

(defn unary-expression
  [[op expr]]
  (let [form (case op
               (+) nil
               (-) `(~'* -1)
               (not) `(~'not)
               (nil) nil)
        ]
    (if form
    `(~@form ~expr)
    expr)))
