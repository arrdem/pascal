(ns me.arrdem.pascal.semantics
  (:require [clojure.pprint :refer [pprint]]

            [me.arrdem.compiler :refer [nameof typeof sizeof fields
                                        valueof follow field-offset]]
            [me.arrdem.compiler.types :refer [->RecordType]]
            [me.arrdem.compiler.macros :refer [pmacroexpand macro?]]
            [me.arrdem.compiler.symtab :refer [genlabel! install!
                                               search gensym! render-ns]]
            [me.arrdem.compiler.symbols :refer [->VariableType ->ArrayType
                                                ->EnumType ->RecordEntry
                                                ->PointerType ->ThinType]]
            [me.arrdem.compiler.symbol-conversions]
            [me.arrdem.pascal.ast :refer :all]
            [name.choi.joshua.fnparse :as fnp]))

(defn tail-cons
  "Basic cons operation for joining recursively defined eliminated lists.
   Eg. a, b, c, d, e -> (a (b (c (d (e nil))))) which this takes down to
   (a b c d e) for ease of use."
  [[s [_ rest]]]
  (cons s rest))

;;------------------------------------------------------------------------------
;; NEW generation code..
;; TODO: organize this in some vaguely sane way so that it relates to the code
;;       in grammar.clj more nicely. Maybe even modularize both the core grammar
;;       and the semantics then inline the sub-groups?

(defn const-assign
  "Creates & installs a typed (generated) variable to represent constants,
   returning a variable identifier which the constant's symbol will shadow."
  [[id _ v]]
  (let [v (->VariableType id (nameof (typeof v)) v)]
    (dbg-install v)))

(defn constant-declaration
  [[_ c0 cs]]
  (let [cs (map second cs)] ;; cs is pairs tok_semi, const
    (apply makecomment "got constant decl group:" (cons c0 cs))))

(defn string
  "Generates an anonymous string variable & returns its identifier so that other
   code can use (and shadow) the constant string val it represents."
  [s]
  (let [sym (->VariableType (gensym! "str_") "string" s)]
    (dbg-install sym)))

(defn label-declaration
  "Generates variables with the _string_ values of labels, integer type and a
   constant value equal to the genlabel! when they were parsed. Installs these
   ``variables'' in the symbol table so that they can be looked up by a special
   case in the goto production code."
  [[_l l0 ls]]
  (let [ls (map second ls)
        labels (map str (cons l0 ls))]
    (doseq [l labels]
      (install! (->VariableType l "integer" (genlabel!))))
    (apply makecomment "found label declarations" labels)))

(defn point-type
  "Stupid little routine to compute the name of a pointer type based on the
   name of the type to which it points."
  [[_point type]]
  (str "^" type))

;;------------------------------------------------------------------------------
;; Compound types...

(defn install-range
  [[low _r high]]
  (let [c (- (inc high) low)
        i "integer"
        t (->RecordType (gensym! (str "range-" low "->" high "_"))
                        (map (fn [x y] {:value x :name x :type y})
                             (range low (inc high))
                             (repeat c i)))]
    (install! t)))

(defn install-enum
  [[_0 idlist _1]]
  (let [c (count idlist)
        i "integer"
        t (->EnumType (gensym! (str "enum-0->" c "_"))
                      (->> [(repeat c i) (range c)]
                           (apply (partial map #(assoc %1 :value %2)))
                           (zipmap idlist))
                      i)
        t (install! t)]
    (doseq [[i j] (map list idlist (range c))]
      (install! (->VariableType i t j)))
    t))

(defn apply-type
  [syms type]
  ;; (println "[apply-type] syms: " syms)
  ;; (println "[apply-type] type: " type)
  (let [t (nameof (typeof type))]
     (map #(->RecordEntry %1 t nil)
          syms)))

(defn build-array [index-range-list basictype]
  ;; (println "[build-array] " index-range-list)
  (loop [index-range-list (->> index-range-list
                               reverse
                               (map search)
                               (map (fn [x] (range (count (fields x))))))
         state (list basictype)]
    ;; (println "[build-array] - " index-range-list)
    ;; (println "[build-array] - " (count state))
    (let [index-list (first index-range-list)
          index-range-list (rest index-range-list)
          type (first state)]
      ;; (println "[build-array] - type - " (nameof type))
      (let [substruct (->RecordType (gensym! "__array_")
                                    (apply-type index-list type))]
        (install! substruct)
        (if-not (empty? index-range-list)
          (recur index-range-list
                 (cons substruct state))
          (cons substruct state))))))

(defn install-arrtype
  "Computes and installs an array type, being a record with multi-integer
   addressed fields."
  [[_arr _0 index-list _1 _of type]]
  ;; (println "[install-arrtype] index-list: " index-list)
  ;; (println "[install-arrtype] type: " type)
  (let [subarray-seq (build-array index-list type)]
    ;; (println "[install-arrtype] build-array returned OK")
    (install! (->ArrayType (gensym! "array_")
                           (sizeof (first subarray-seq))
                           subarray-seq))))

(defn record-field
  [[idlist _ type]]
  (apply-type idlist type))

(defn install-record
  [[_tr field-list _tend]]
  (let [members (reduce concat field-list)
        t (->RecordType (gensym! "record__")
                        members)]
    (install! t)))

(defn install-reftype
  [[_opt id]]
  (let [name (str "^" id)
        entry (->PointerType name 8 id)]
    (install! entry)))

(defn install-type
  [[id _ type]]
  (install! (->ThinType id type)))

(defn type-declaration
  [[_t decls _e]]
  (apply makecomment "got type definitions:" (map #(. %1 nameof) decls)))

;;------------------------------------------------------------------------------
;; Variables....
(defn vardecl
  "Semantics for the vardecl rule in grammar.clj.
   Enters vars with their types in the symbol table."
  [[varseq _ type]]
  (doseq [v varseq]
    (let [v (->VariableType v (nameof type) nil)]
      (dbg-install v)))
  (map abs-name varseq))

(defn variable-declaration
  "Invoked to generate the comment group for variable declaration parts"
  [[_ decls]]
  (apply makecomment "defined variables" (reduce concat decls)))

(defn var-dot [[_dot id]]
  (fn [x]
    (list (list '. id)
          (get (fields x) id))))

(defn var-point [_]
  (fn [obj]
    (println "; [var-point] " (nameof obj) " is " (nameof (follow obj) ))
    (list (list (symbol "^"))
          (follow obj))))

(defn var-index
  [[_lb subscripts _rb]]
  (fn [obj]
    (list (partial-make-aref
           (field-offset
            obj (seq subscripts)))
          (typeof (last (fields obj))))))

(defn variable
  "Generates the appropriate aref & pointer expressions for indexing into a
   variable of some type. Postfixes is assumed to be a sequence of
   single-argument functions which expect the symbol value of the variable as
   their only argument and which expand into index operations such as aref and
   the deference operator."
  [[id postfixes]]
  (if-let [self (search id)]
    (let [res (reduce (fn [state f]
                        (println "; [variable-fn] f:" f)
                        (let [[expr new-state] (f (:sym state))]
                          (-> state
                              (assoc :sym new-state)
                              (update-in [:ops] conj expr))))
                      {:sym self} postfixes)]
      (assert search)
      ;; (println "; [variable] " id)
      ;; (println "; [variable] " (:ops res))
      (if-not (empty? postfixes)
        (apply e-> id (reverse (:ops res)))
        id))))

;;------------------------------------------------------------------------------
;; Arithmetic expressions...
(defn additive-expression
  [[me tail]]
  (if-let [[op adxpr] tail]
    (binop me op adxpr)
    me))

(defn assignment
  [[target assignop expr]]
  (binop target ':= expr))

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

;;------------------------------------------------------------------------------
;; Basic program control structure transforms
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
  (println "; in block2progn..")
  (->> exprs
      (remove nil?)
      (makeprogn-v)
      (pmacroexpand)))

;;;;----------------------------------------------------------------------------
;;;; the FOR control structure
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

;;;;----------------------------------------------------------------------------
;;;; the REPEAT control structure
(defn repeat-stmnt
  [[_rep stmnts _unt expr]]
  (let [lbl (genlabel!)]
    (makeprogn
     [(makelabel lbl)
      (makeprogn stmnts)
      (makeif `(~'not ~expr)
              (makegoto lbl))
      ])))

;;;;----------------------------------------------------------------------------
;;;; WHILE structure
(defn while-stmnt
  [[_while e _do stmnt]]
  (makewhile e stmnt))

;;;;----------------------------------------------------------------------------
;;;; IF structures
(defn ift
  [[_if expr _then stmnt]]
  (makeif expr stmnt))

(defn ifte
  [[_if expr _then stmnt _else stmnt2]]
  (makeif expr stmnt stmnt2))

;;;;----------------------------------------------------------------------------
;;;; Other statement parts...
(defn procinvoke
  [[id [_0 params _1]]]
  (let [f (search id)]
    (if (macro? f)
      (concat (list (nameof f))
              params)
      (makefuncall id params))))

(defn statements
  [[_0 stmnts _1]]
  (makeprogn stmnts))

(defn label-stmnt
  [[lbl _colon stmnt]]
  (makeprogn
   (list
    (-> lbl
        str
        search
        valueof
        makelabel)
    stmnt)))

(defn goto-label
  [[_gt lbl]]
  (-> lbl
      str
      search
      valueof
      makegoto))
