(ns me.arrdem.pascal.semantics
  (:require [clojure.pprint :refer [pprint]]

            [me.arrdem.compiler.symbols :refer [->VariableType ->RecordType
                                                nameof typeof ->ArrayType
                                                sizeof fields]]
            [me.arrdem.compiler.symtab :refer [genlabel! install!
                                               search gensym! render-ns]]
            [me.arrdem.pascal.ast :refer :all]

            [name.choi.joshua.fnparse :as fnp]))

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
;; NEW generation code..

(defn vardecl
  "Semantics for the vardecl rule in grammar.clj.
   Enters vars with their types in the symbol table."
  [[varseq _ type]]
  (doseq [v varseq]
    (let [v (->VariableType v (search (nameof type)) nil)]
      (dbg-install v)))
  (map abs-name varseq))

(defn variable-declaration
  [[_ decls]]
  (apply makecomment "defined variables" (reduce concat decls)))

(defn const-assign
  [[id _ v]]
  (let [v (->VariableType id (search (typeof v)) v)]
    (dbg-install v)))

(defn constant-declaration
  [[_ c0 cs]]
  (let [cs (map second cs)] ;; cs is pairs tok_semi, const
    (apply makecomment "got constant decl group:" (cons c0 cs))))

(defn string
  [s]
  (let [sym (->VariableType (gensym! "str_") "string" s)]
    (dbg-install sym)))

(defn snum
  [prefix [sign? rval]]
  (let [factor (case sign?
                 (+ nil) 1
                 (-) -1)
        sym (gensym! (str prefix "_"))
        val (->VariableType sym (typeof rval) (* factor rval))]
    (dbg-install val)))

(def integer (partial snum "integer"))
(def real (partial snum "real"))

(defn label-declaration
  [[_l l0 ls]]
  (let [ls (map second ls)]
    (makecomment "found label declarations" (cons l0 ls))))

(defn variable
  [[id postfixes]]
  (let [id (abs-name id)]
    (assert id)
    (if-not (empty? postfixes)
      (apply (partial e-> id) postfixes)
      id)))

(defn additive-expression
  [[me tail]]
  (if-let [[op adxpr] tail]
    (binop me op adxpr)
    me))

(defn assignment
  [[target assignop expr]]
  (binop target ':= expr))

(defn pascal-program
  [[[_0 id] heading _1 block _2]]
  `(~'program ~id ~@heading ~@block))

(defn program-heading
  [[_l ids _r]]
  (map makeprogn-v
       (map list ids)))

(defn block
  [[comments progn]]
  (concat comments (list progn)))

(defn block2progn
  [[_0 exprs _1]]
  (makeprogn-v (remove nil? exprs)))

(defn for-downto
  [[s0 _ sf]]
  [s0 `(~'- 1) '>= sf])

(defn for-to
  [[s0 _ sf]]
  [s0 `(~'+ 1) '<= sf])

(defn for-stmnt [[_0 id _1 flist _3 stmnt]]
  (let [[Vi update comp end] flist
        lstart (genlabel!)
        id     (abs-name (search id))]
    (makeprogn
      [(makelabel lstart)
       (binop id ':= Vi)
       (makeif `(~comp ~id ~end)
               (makeprogn [stmnt
                           (binop id ':= (concat update (list id)))
                           (makegoto lstart)]))
       ])))

(defn repeat-stmnt
  [[_rep stmnts _unt expr]]
  (let [lbl (genlabel!)]
    (makeprogn
     [(makelabel lbl)
      (makeprogn stmnts)
      (makeif `(~'not ~expr)
              (makegoto lbl))
      ])))

(defn procinvoke
  [[id [_0 params _1]]]
  (makefuncall id params))

(defn identifier
  ([id] (abs-name id)))

(defn unary-expression
  [[op expr]]
  (let [form (case op
               (+) nil
               (-) `(~'* -1)
               (not) `(~'not)
               (nil) nil)]
    (if form
      (concat form (list expr))
      expr)))

(defn statements
  [[_0 stmnts _1]]
  (makeprogn stmnts))

(defn point-type
  [[_point type]]
  (str "^" type))

(defn var-index
  [[_lb subscripts _rb]]
  (map partial-make-aref
       (if (seq? subscripts)
         subscripts [subscripts])))

(defn install-arrtype
  [[_arr _0 index-list _1 _of type]]
  (loop [t (reverse index-list)
         child-type (search type)]
    (let [my-ind (first t)
          my-ind (if (instance? me.arrdem.compiler.symbols.RecordType  my-ind)
                   my-ind (typeof (search my-ind)))
          my-len  (count (fields my-ind))
          my-name (str (nameof child-type) "-" my-len)
          self (->ArrayType
                 (str (nameof child-type) "-" my-len)
                 (* (sizeof child-type) my-len)
                 (zipmap (keys (fields my-ind))
                         (repeat my-len child-type)))]
      (install! self)
      (if-not (empty? (rest t))
        (recur (rest t)
               self)
        self))))

(defn install-range
  [[low _r high]]
  (let [c (- (inc high) low)
        i (search "integer")
        t (->RecordType (gensym! (str "range-" low "->" high "_"))
                        (zipmap (range low (inc high))
                                (repeat c i)))]
    (install! t)))
