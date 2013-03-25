(ns me.arrdem.pascal.grammar
  (:require [me.arrdem.pascal.tokens :refer :all :except [pascal-base]]
            [me.arrdem.pascal.symtab :refer [install! descend! ascend!]]
            [me.arrdem.pascal.semantics :refer [binop makeif makederef
                                                makewhile makerepeat
                                                makeupfor makedownfor]]
            [me.arrdem.pascal.hooks :refer [defrule]]0
            [name.choi.joshua.fnparse :as fnp]))

;; declared here
(declare
   Assignment Program Var-header Varlist
   Statements Statement Identlist Expr
   Factor Term Type PNumber Const-header)

(defrule Program
  (fnp/semantics
   (fnp/conc tok_program
             (fnp/semantics identifier
              (fn [i] (descend! i) i))
              (fnp/semantics
               (fnp/conc delim_lparen
                         Identlist
                         delim_rparen)
               second)
              delim_semi
              (fnp/opt Const-header)
              (fnp/opt Var-header)
               Statements
             op_dot)
   (fn [[_0 i v _1 _2 _3 s _4]]
     `(~'program ~i
                 ~@(map (fn [p] `(~'do ~p)) v)
                 ~s))))

(defrule Const-header
  (fnp/conc tok_const
            (fnp/rep+
             (fnp/semantics
              (fnp/conc identifier
                        op_eq
                        PNumber
                        delim_semi)
              (fn [[i _ v _d]]
                (let [v (assoc v :name i)]
                  (install! v)
                  v))))))

(defrule Var-header
  (fnp/semantics
   (fnp/conc tok_var
             (fnp/rep+
              (fnp/semantics
               (fnp/conc
                (fnp/semantics
                 (fnp/conc Identlist
                           delim_colon
                           identifier)
                 (fn [[ids _ t]]
                   (doseq [i ids]
                     (install! {:name i
                                :type :symbol
                                :type/data t}))
                   ids))
                delim_semi)
               first)))
   (comp flatten second)))

(defrule Varlist
  (fnp/semantics
   (fnp/alt
    (fnp/conc Identlist
              delim_colon
              identifier
              delim_semi
              Varlist)

    (fnp/conc Identlist
              delim_colon
              identifier))

   (fn [[syms _ t]]
     (doseq [sym syms]
       (-> {}
           (assoc :type/data t)
           (assoc :type :symbol)
           (assoc :name sym)
           install!)))))

(defrule Identlist
  (fnp/alt
   (fnp/semantics
    (fnp/conc identifier
              delim_comma
              Identlist)
    (fn [[a _ b]]
      (cons a b)))
   (fnp/semantics
    identifier
    list)))

(defrule Statements
  (fnp/semantics
   (fnp/conc tok_begin
             Statement
             (fnp/rep*
              (fnp/semantics
               (fnp/conc delim_semi
                         Statement)
               second))
             tok_end)
    (fn [[_0 s others _1]]
      `(~'do ~s ~@others))))

(defrule Statement
  (fnp/alt
   ;; IF expression
   (fnp/semantics
    (fnp/conc tok_if
              Expr
              tok_then
              Statement
              (fnp/opt
               (fnp/conc tok_else
                         Statement)))
    makeif)

   ;; FOR expression
   (fnp/semantics
    (fnp/conc tok_for
              identifier
              op_assign
              Expr
              tok_to
              Expr
              tok_do
              Statements)
    makeupfor)

   (fnp/semantics
    (fnp/conc tok_for
              identifier
              op_assign
              Expr
              tok_downto
              Expr
              tok_do
              Statements)
    makedownfor)


   ;; WHILE expression
   (fnp/semantics
    (fnp/conc tok_while
              Expr
              tok_do
              Statement)
    makewhile)

   ;; REPEAT expression
   (fnp/semantics
    (fnp/conc tok_repeat
              Statement
              tok_until
              Expr)
    makerepeat)

   ;; Assignment expressions
   Assignment))

(defrule Assignment
  (fnp/semantics
   (fnp/conc (fnp/semantics
              identifier
              makederef)
             op_assign
             Expr)
   binop))

(defrule Expr
  (fnp/alt
   ;; Addition
   (fnp/semantics
    (fnp/conc Term
              (fnp/alt (fnp/constant-semantics op_add '+)
                       (fnp/constant-semantics op_sub '-)
                       (fnp/constant-semantics op_or  'or)
                       (fnp/constant-semantics op_eq  '=))
              Expr)
    binop)
   ;; identity
   Term))

(defrule Term
  (fnp/alt
   (fnp/semantics
    (fnp/conc Factor
              (fnp/alt (fnp/constant-semantics op_mul '*)
                       (fnp/constant-semantics op_div '/)
                       (fnp/constant-semantics op_mod 'mod)
                       (fnp/constant-semantics op_and 'and))
              Term)
    binop)
   Factor))

(defrule Factor
  (fnp/alt
   (fnp/semantics
    identifier
    (fn [id]
      (makederef id)))
   (fnp/semantics
    PNumber
    :value)
   (fnp/semantics
    (fnp/conc delim_lparen
              Expr
              delim_rparen)
    second)))

(defrule PNumber
  (fnp/alt
   (fnp/semantics
    floatnum
    (fn [x]
      {:type "real"
       :value x}))

   (fnp/semantics
    intnum
    (fn [x]
      {:type "integer"
       :value x}))))
