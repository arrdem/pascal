(ns me.arrdem.pascal.semantics
  (:require [me.arrdem.compiler :refer [nameof typeof sizeof fields
                                        valueof follow field-offset addrof
                                        return-type valid-invokation?]]
            [me.arrdem.compiler.macros :refer [pmacroexpand macro?]]
            [me.arrdem.compiler.symtab :refer [genlabel! install!
                                               search gensym! render-ns
                                               ->qname]]
            [me.arrdem.compiler.ast :refer :all]
            [me.arrdem.pascal.types :refer [convert level]]
            (me.arrdem.compiler.symbols
               [primitives]
               [records :refer [->RecordType ->RecordEntry]]
               [core :refer [->PointerType]]
               [complex :refer [->EnumType ->ThinType ->RangeType
                                ->VariableType ->ArrayType]])))

(defn tail-cons
  "Basic cons operation for joining recursively defined eliminated lists.
   Eg. a, b, c, d, e -> (a (b (c (d (e nil))))) which this takes down to
   (a b c d e) for ease of use."
  [[s [_ rest]]]
  (cons s rest))

(defn binop
  "Computes a typed arithmetic expression for two arguments and an operator.
   Serves as a portal through which all arithmetic must pass and thus provides
   almost all required type conversion silently."
  [e0 op e1]
  (case [op e0 e1]
    (['+ nil nil]) 1
    (['* nil nil]) 1
    (['- nil nil]) 0
    (['/ nil nil]) 1
    (do
      ;; (println "; [binop] " e0 op e1)
      ;; (println "; [binop]" (nameof (typeof e0)) op (nameof (typeof e1)))
      (let [lvlval (level e0 e1)]
        (with-meta
          `(~op ~@lvlval)
          {:type (->> lvlval
                      (remove nil?)
                      (map typeof)
                      first)})))))
(defn makeassign
  "Builds an assignment statement forcing the type of the second expression to
   that of the first expression. Previously a special case of binop."
  [e0 e1]
  `(~':= ~e0 ~(convert e1
                       (nameof (typeof e1))
                       (nameof (typeof e0)))))

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
    (nameof (install! v))))

(defn constant-declaration
  [[_ c0 cs]]
  (let [cs (map second cs)] ;; cs is pairs tok_semi, const
    (apply makecomment "got constant decl group:" (cons c0 cs))))

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
        t (->RangeType (gensym! (str "__range-" low "->" high "_"))
                       (range low (inc high)))]
    (nameof (install! t))))

(defn install-enum
  [[_0 idlist _1]]
  (let [c (count idlist)
        i "integer"
        t (->EnumType (gensym! (str "__enum-0->" c "_"))
                      (->> [(repeat c i) (range c)]
                           (apply (partial map #(-> {}
                                                    (assoc :name %1)
                                                    (assoc :value %2))))
                           (zipmap (map ->qname idlist)))
                      i)
        t (nameof (install! t))]
    (doseq [[i j] (map list idlist (range c))]
      (install! (->VariableType i t j)))
    t))

(defn make-array-name [type count]
  (format  "array<%s>[%s]" type count))

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
                               (map valueof))
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
                 (cons (nameof substruct) state))
          (cons (nameof substruct) state))))))

(defn ensure-array-type! [type count]
  (let [name (make-array-name type count)]
    (or (try (search name) (catch Exception e nil))
        (install! (->ArrayType name
                               (* count (sizeof type))
                               {})))))

(defn string
  "Generates an anonymous string variable & returns its identifier so that other
   code can use (and shadow) the constnt string val it represents."
  [s]
  (let [type (ensure-array-type! "alpha" (count s))
        sym (->VariableType (gensym! "__str_") type s)]
    (nameof (install! sym))))

(defn install-arrtype
  "Computes and installs an array type, being a record with multi-integer
   addressed fields."
  [[_arr _0 index-list _1 _of type]]
  ;; (println "[install-arrtype] index-list: " index-list)
  ;; (println "[install-arrtype] type: " type)
  (->> (build-array index-list type)
       (#(->ArrayType (->> index-list
                           (map (comp count
                                      :range
                                      search))
                           (interpose ",")
                           (apply str)
                           (make-array-name type))
                    (sizeof (first %1))
                    %1))
       (install!)
       (nameof)))

(defn record-field
  [[idlist _ type]]
  (apply-type idlist type))

(defn record-name [members]
  (str "record{" (->> members
                      (map typeof)
                      (interpose ",")
                      (apply str))
       "}"))

(defn install-record
  [[_tr field-list _tend]]
  (let [members (reduce concat field-list)
        t (->RecordType (record-name members)
                        members)]
    (nameof (install! t))))

(defn install-reftype
  [[_opt id]]
  (let [name (str "^" id)
        entry (->PointerType name 8 id)]
    (nameof (install! entry))))

(defn install-type
  [[id _ type]]
  (->> type
       (->ThinType id)
       (install!)
       (nameof)))

(defn type-declaration
  [[_t decls _e]]
  (apply makecomment
         "got type definitions:"
         (map nameof decls)))

;;------------------------------------------------------------------------------
;; Variables....
(defn vardecl
  "Semantics for the vardecl rule in grammar.clj.
   Enters vars with their types in the symbol table."
  [[varseq _ type]]
  (doseq [v varseq]
    (let [v (->VariableType v (nameof type) nil)]
      (install! v)))
  (map (comp nameof search) varseq))

(defn variable-declaration
  "Invoked to generate the comment group for variable declaration parts"
  [[_ decls]]
  (apply makecomment
         "defined variables"
         (reduce concat decls)))

(defn var-dot [[_dot id]]
  (fn [obj]
    (if-not (satisfies? me.arrdem.compiler/IIndexable obj)
      (do (println "[VAR-DOT] got argument" (pr-str obj))
          (println "[VAR-DOT] NOT PROVIDING IIndexable")
          (assert false)))
    (let [val (get (fields obj) id)
          res (nameof (typeof val))]
      ;; (println "; [var-dot] " (nameof obj)
      ;;          " is " res)
      (list (list 'aref (addrof val))
            res))))

(defn var-point [_]
  (fn [obj]
    (assert (satisfies? me.arrdem.compiler/IPointer obj)
            (str "type " obj " does not appear to be IPointer"))
    (assert (not (nil? (follow obj))))
    (let [res (typeof (follow obj))]
      ;; (println "; [var-point] " (nameof obj) " is " res)
      (makederef res))))

(defn- var-compute-index [field type subscript]
  (or (if-let [field-entry (get (fields field) subscript)]
        (addrof field-entry))
      (binop (sizeof type) '* subscript)))

(defn var-index
  [[_lb subscripts _rb]]
  (fn [obj]
    (assert (satisfies? me.arrdem.compiler/IIndexable obj))
    ;; (println "; [var-index] " (nameof obj) " is " (nameof (typeof (last (fields obj)))))
    (let [index-seq (map var-compute-index
                        (fields obj)
                        (next (fields obj))
                        subscripts)]
      ;; (println "; [var-index]" index-seq)
      (list (partial-make-aref
             ;; TODO: rework this in terms of binop somehow..
             (reduce #(binop %1 '+ %2) 0
                     index-seq))
             (last (fields obj))))))

(defn variable
  "Generates the appropriate aref & pointer expressions for indexing into a
   variable of some type. Postfixes is assumed to be a sequence of
   single-argument functions which expect the symbol value of the variable as
   their only argument and which expand into index operations such as aref and
   the deference operator."
  [[id postfixes]]
  (if-let [self (search id)]
    (let [res (reduce (fn [state f]
                        ;; (println "; [variable-fn] f:" f)
                        (let [[expr new-state] (f (:sym state))]
                          (-> state
                              (assoc :sym new-state)
                              (update-in [:ops] conj expr))))
                      {:sym self} postfixes)]
      (assert self)
      ;; (println "; [variable] " id)
      ;; (println "; [variable] " (:ops res))
      (if-not (empty? postfixes)
        (with-meta
          (apply e-> (nameof self) (reverse (:ops res)))
          {:type (:sym res)})
        (nameof self)))))

;;------------------------------------------------------------------------------
;; Arithmetic expressions...
(defn additive-expression
  [[me tail]]
  (if-let [[op adxpr] tail]
    (binop me op adxpr)
    me))

(defn assignment
  [[target _assignop expr]]
  (makeassign target expr))

(defn unary-expression
  [[op expr]]
  ((case op
     (-) (partial binop -1 '*)
     (not) binnot
     identity)
   expr))

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
  ;; (println "; in block2progn..")
  (->> exprs
      (remove nil?)
      (makeprogn-v)
      (pmacroexpand)))

;;;;----------------------------------------------------------------------------
;;;; the FOR control structure
(defn for-downto
  [[s0 _ sf]]
  [s0 '- '>= sf])

(defn for-to
  [[s0 _ sf]]
  [s0 '+ '<= sf])

(defn for-stmnt [[_0 id _1 flist _3 stmnt]]
  (let [[Vi update comp end] flist
        lstart (genlabel!)
        id (nameof (search id))]
    (makeprogn
      [(makelabel lstart)
       (makeassign id Vi)
       (makeif `(~comp ~id ~end)
               (makeprogn [stmnt
                           (makeassign id (binop id update 1))
                           (makegoto lstart)]))])))

;;;;----------------------------------------------------------------------------
;;;; the REPEAT control structure
(defn repeat-stmnt
  [[_rep stmnts _unt expr]]
  (let [lbl (genlabel!)]
    (makeprogn
     [(makelabel lbl)
      (makeprogn stmnts)
      (makeif `(~'not ~expr)
              (makegoto lbl))])))

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
  (if (macro? id)
    (concat (list (nameof id))
            params)
    (do
      (assert (valid-invokation? (search id)
                                 (apply list
                                        (map typeof params))))
      (with-meta
        (makefuncall id params)
        {:type (return-type (search id))}))))

(defn statements
  [[_0 stmnts _1]]
  (makeprogn stmnts))

(defn label-stmnt
  [[lbl _colon stmnt]]
  (makeprogn
   (list
    (-> lbl
        str
        valueof
        makelabel)
    stmnt)))

(defn goto-label
  [[_gt lbl]]
  (-> lbl
      str
      valueof
      makegoto))
