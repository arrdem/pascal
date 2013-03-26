(ns me.arrdem.pascal.grammar
  (:require [me.arrdem.pascal.tokens :refer :all :except [pascal-base]]
            [me.arrdem.pascal.symtab :refer [install! descend! ascend!]]
            [me.arrdem.pascal.semantics :refer [binop makeif makederef
                                                makewhile makerepeat
                                                makeupfor makedownfor
                                                makefuncall]]
            [me.arrdem.pascal.debug :refer [debug]]
            [me.arrdem.pascal.hooks :refer [defrule]]
            [name.choi.joshua.fnparse :refer :all]))

(defmacro fail-sem
  ([rule succ-fn]
     `(semantics ~rule
                 ~succ-fn))
  ([rule succ-fn fail-fn]
     `(failpoint (semantics ~rule
                            ~succ-fn)
                 ~fail-fn)))

(defmacro p [& rest]
  `(fn [& _#]
     (me.arrdem.pascal.debug/debug ~@rest)))

;; declared here
(declare
   Assignment Program Var-header Varlist
   Statements Statement Identlist Expr
   Factor Term Type PNumber Const-header)

(defrule Program
  (semantics
   (conc tok_program
         (fail-sem
          identifier
          (fn [i] (descend! i) i)
          (p "[Program] failed to find a program identifier"))
         (fail-sem
          (conc delim_lparen
                Identlist
                delim_rparen)
          second
          (p "[Program] failed to find a program params group"))
         (failpoint delim_semi
          (p "[Program] failed to find a terminating semicol"))
         (failpoint Const-header
          (p "[Program] failed to find a constdecl header"))
         (failpoint Var-header
          (p "[Program] failed to find a vardecl header"))
         (failpoint Statements
          (p "[Program] failed to find a statement sequence"))
         (failpoint op_dot
          (p "[Program] no trailing dot, n00b mistake")))
   (fn [[_0 i v _1 _2 _3 s _4]]
     `(~'program ~i
                 ~@(map (fn [p] `(~'do ~p)) v)
                 ~s))))

(defrule Const-header
  (alt
   (conc tok_const
         (semantics
          (rep+
           (semantics
            (conc identifier
                  op_eq
                  PNumber
                  delim_semi)
            (fn [[a b c d]] (list a c))))
          (fn [defs]
            (debug "lex OK, installing IDs")
            (doseq [d defs]
             (let [[i v] d
                   v (assoc v :name i)]
               (install! v)
               (debug (str "installed:" i " value " v)))))))
   emptiness))

(defrule Var-header
  (alt
   (semantics
    (conc tok_var
          (rep+
           (semantics
            (conc
             (semantics
              (conc Identlist
                    delim_colon
                    identifier)
              (fn [[ids _ t]]
                (debug "lex OK, installing IDs")
                (doseq [i ids]
                  (install! {:name i
                             :type :symbol
                             :type/data t}))
                ids))
             delim_semi)
            first)))
    (comp flatten second))
   emptiness))

(defrule Varlist
  (semantics
   (alt
    (conc Identlist
              delim_colon
              identifier
              delim_semi
              Varlist)

    (conc Identlist
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
  (alt
   (semantics
    (conc identifier
              delim_comma
              Identlist)
    (fn [[a _ b]]
      (cons a b)))
   (semantics
    identifier
    list)))

(defrule Statements
  (semantics
   (conc (failpoint tok_begin
          (fn [& a]
            (debug "failed, no begin token" a)))
         (failpoint Statement
          (p "failed, no zero statement"))
         (rep*
          (semantics
           (conc delim_semi
                 Statement)
           second))
         (failpoint tok_end
          (p "failed, no end token")))
    (fn [[_0 s others _1]]
      `(~'do ~s ~@others))))

(defrule Statement
  (alt
   ;; IF expression
   (semantics
    (conc tok_if
              Expr
              tok_then
              Statement
              (opt
               (conc tok_else
                         Statement)))
    makeif)

   ;; FOR expression
   (semantics
    (conc tok_for
              identifier
              op_assign
              Expr
              tok_to
              Expr
              tok_do
              Statements)
    makeupfor)

   (semantics
    (conc tok_for
              identifier
              op_assign
              Expr
              tok_downto
              Expr
              tok_do
              Statements)
    makedownfor)


   ;; WHILE expression
   (semantics
    (conc tok_while
              Expr
              tok_do
              Statement)
    makewhile)

   ;; REPEAT expression
   (semantics
    (conc tok_repeat
              Statement
              tok_until
              Expr)
    makerepeat)

   ;; Assignment expressions
   Assignment))

(defrule Assignment
  (semantics
   (conc (semantics
              identifier
              makederef)
             op_assign
             Expr)
   binop))

(defrule Expr
  (alt
   ;; Addition
   (semantics
    (conc Term
              (alt (constant-semantics op_add '+)
                       (constant-semantics op_sub '-)
                       (constant-semantics op_or  'or)
                       (constant-semantics op_eq  '=))
              Expr)
    binop)
   ;; identity
   Term))

(defrule Term
  (alt
   (semantics
    (conc Factor
              (alt (constant-semantics op_mul '*)
                       (constant-semantics op_div '/)
                       (constant-semantics op_mod 'mod)
                       (constant-semantics op_and 'and))
              Term)
    binop)
   Factor))

(defrule Funcall
  (semantics
   (conc identifier
         delim_lparen
         (semantics
          (conc Expr
                (rep*
                 (conc delim_comma Expr)))
          (fn [[e0 erest]] (conj erest e0)))
         delim_rparen)
   makefuncall))

(defrule Factor
  (semantics
   (conc (opt (alt op_add op_sub))
         (alt
          Funcall
          (semantics
           identifier
           (fn [id]
             (makederef id)))
          PNumber
          (semantics
           (conc delim_lparen
                 Expr
                 delim_rparen)
           second)))
   (fn [[o n]]
     (case o
       (nil +) n
       (-) `(~'- 0 ~n)))))

  (defrule PNumber
  (alt
   (semantics
    floatnum
    (fn [x]
      {:type "real"
       :val x}))

   (semantics
    intnum
    (fn [x]
      {:type "integer"
       :val x}))))
