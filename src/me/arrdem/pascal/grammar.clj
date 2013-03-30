(ns me.arrdem.pascal.grammar
  (:require [me.arrdem.pascal.tokens :refer :all]
            [me.arrdem.pascal.semantics :as s]
            [me.arrdem.pascal.symtab :refer [ascend! descend!]]

            [name.choi.joshua.fnparse :as fnp]
            ))

;;------------------------------------------------------------------------------
;; Forward declarations of rules generated by sad
(declare
 label-declaration unary-expression subscript-list proc-or-func label
 pascal-program simple-type element-list for-list block5 procid case-list
 block-or-forward element variable constant record-field expression
 fixed-part variable-declaration variableid-list case-label-list
 primary-expression fieldid identifier-list field-list block3
 parameterid-list relational-op block4 block1 block2 funcid structured-type
 formal-parameter-list block formal-parameter-section statement-list
 variant-list type-declaration constid expression-list
 proc-and-func-declaration unary-op fieldid-list parameters varid typeid
 statement multiplicative-expression additive-op variant-part
 constant-declaration tag-field ptype record-variable-list program-heading
 additive-expression index-list variant multiplicative-op)

;;------------------------------------------------------------------------------
;; Grammar terminals & nonterminals which were not defined in the grammar

;; (def identifier
;;   (fnp/semantics
;;    me.arrdem.pascal.tokens/identifier
;;    s/identifier))

(def unsigned-integer intnum)
(def unsigned-real floatnum)

(def string
  (fnp/semantics
   pstring
   s/string))

(def integer
  unsigned-integer)

(def real
  unsigned-real)

;;------------------------------------------------------------------------------
;; The compiled grammar almost exactly as generated by sad

(def prognid
  (fnp/semantics
   (fnp/conc tok_program
             identifier)
   (fn [[_ id]]
     (println "; dropping into program" id)
     (descend! id)
     [_ id])))

(def pascal-program
  (fnp/semantics
   (fnp/conc prognid
             program-heading
             delim_semi
             block
             op_dot)
   s/pascal-program))

(def program-heading
  (fnp/semantics
   (fnp/conc delim_lparen
             identifier-list
             delim_rparen)
   s/program-heading))

(def identifier-list
  (fnp/semantics
   (fnp/conc identifier
             (fnp/opt
              (fnp/conc delim_comma
                        identifier-list)))
   s/tail-cons))

(def block
  (fnp/semantics
   (fnp/conc
    (fnp/semantics
     (fnp/conc
      (fnp/opt (fnp/conc label-declaration delim_semi))
      (fnp/opt (fnp/conc constant-declaration delim_semi))
      (fnp/opt (fnp/conc type-declaration delim_semi))
      (fnp/opt (fnp/conc variable-declaration delim_semi))
      (fnp/opt (fnp/conc proc-and-func-declaration delim_semi)))
     (comp (partial remove nil?)
           (partial map first)))
    (fnp/semantics
     (fnp/conc tok_begin statement-list tok_end)
     s/block2progn))
   s/block))

(def label-declaration
  (fnp/semantics
   (fnp/conc tok_label
             unsigned-integer
             (fnp/rep*
              (fnp/conc
               delim_comma
               unsigned-integer)))
   s/label-declaration))

(def const-assign
  (fnp/semantics
   (fnp/conc identifier
             op_eq
             constant)
   s/const-assign))

(def constant-declaration
  (fnp/semantics
   (fnp/conc tok_const
             const-assign
             (fnp/rep*
              (fnp/conc
               delim_semi
               const-assign)))
   s/constant-declaration))

(def type-declaration
  (fnp/alt
   (fnp/conc identifier
             op_eq
             ptype
             delim_semi
             type-declaration)
   (fnp/conc tok_type
             identifier
             op_eq
             ptype)))

(def vardecl
  (fnp/semantics
   (fnp/conc variableid-list
             delim_colon
             ptype)
   s/vardecl))

(def vardecls
  (fnp/semantics
   (fnp/conc vardecl
             (fnp/opt
              (fnp/conc delim_semi vardecls)))
   s/vardecls))

(def variable-declaration
  (fnp/semantics
   (fnp/conc tok_var
             vardecls)
   s/variable-declaration))

;; variableid-list is now OK, consistently returns a pair [id ids?] where
;; ids? may be nil. semantics also in place.
(def variableid-list
  (fnp/semantics
   (fnp/conc identifier
             (fnp/opt
              (fnp/conc delim_comma
                        variableid-list)))
   s/tail-cons))

(def constant
  (fnp/alt integer
           real
           string
           constid
           (fnp/conc op_add constid)
           (fnp/conc op_sub constid)))

(def ptype
  (fnp/alt simple-type
           structured-type
           (fnp/conc op_point typeid)))

(def simple-type
  (fnp/alt
   (fnp/conc delim_lparen
             identifier-list
             delim_rparen)
   (fnp/conc constant
             delim_dotdot
             constant)
   typeid))

(def structured-type
  (fnp/alt
   (fnp/conc tok_array delim_lbrack index-list delim_rbrack tok_of ptype)
   (fnp/conc tok_record field-list tok_end)
   (fnp/conc tok_set tok_of simple-type)
   (fnp/conc tok_file tok_of ptype)
   (fnp/conc tok_packed structured-type)))

(def index-list
  (fnp/alt (fnp/conc simple-type
                     delim_comma
                     index-list)
           simple-type))

(def field-list
  (fnp/alt
   (fnp/conc fixed-part delim_semi variant-part)
   fixed-part
   variant-part))

(def fixed-part
  (fnp/alt record-field
           (fnp/conc fixed-part
                     delim_semi
                     record-field)))

(def record-field
  (fnp/alt (fnp/conc fieldid-list
                     delim_colon
                     ptype)
           fnp/emptiness))

(def fieldid-list
  (fnp/alt (fnp/conc identifier delim_comma fieldid-list)
           identifier))

(def variant-part
  (fnp/conc tok_case
            tag-field
            tok_of
            variant-list))

(def tag-field
  (fnp/alt typeid
           (fnp/conc identifier
                     delim_colon
                     typeid)))

(def variant-list
  (fnp/alt variant
           (fnp/conc variant-list
                     delim_semi
                     variant)))

(def variant
  (fnp/alt
   (fnp/conc case-label-list
             delim_colon
             delim_lparen
             field-list
             delim_rparen)
   fnp/emptiness))

(def case-label-list
  (fnp/alt
   (fnp/conc constant
             delim_comma
             case-label-list)
   constant))

(def proc-and-func-declaration
  (fnp/alt proc-or-func
           (fnp/conc proc-or-func
                     delim_semi
                     proc-and-func-declaration)))

(def parameters?
  (fnp/opt parameters))

(def proc-or-func
  (fnp/alt
   (fnp/conc tok_procedure
             identifier
             parameters?
             delim_semi
             block-or-forward)
   (fnp/conc tok_function
             identifier
             parameters?
             delim_colon
             typeid
             delim_semi
             block-or-forward)))

(def block-or-forward
  (fnp/alt block
           tok_forward))

(def parameters
  (fnp/conc delim_lparen
            formal-parameter-list
            delim_rparen))

(def formal-parameter-list
  (fnp/alt (fnp/conc formal-parameter-section
                     delim_semi
                     formal-parameter-list)
           formal-parameter-section))

(def formal-parameter-section
  (fnp/alt
   (fnp/conc parameterid-list delim_colon typeid)
   (fnp/conc tok_var parameterid-list delim_colon typeid)
   (fnp/conc tok_procedure identifier (fnp/opt parameters))
   (fnp/conc tok_function identifier (fnp/opt parameters) delim_colon typeid)))

(def parameterid-list
  (fnp/alt (fnp/conc identifier delim_comma parameterid-list)
           identifier))

;;------------------------------------------------------------------------------
;; a bunch of rules which were pulled out of statement to make it easier to
;; build hook and semantics operations for them.

(def statement-list
  (fnp/semantics
   (fnp/conc statement
             (fnp/opt
              (fnp/conc delim_semi statement-list)))
   s/cons-ht))

(def assignment
  (fnp/semantics
   (fnp/conc variable op_assign expression)
   s/assignment))

(def statements
  (fnp/semantics
   (fnp/conc tok_begin statement-list tok_end)
   second))

(def ifte
  (fnp/conc tok_if
            expression
            tok_then
            statement
            tok_else
            statement))

(def ift
  (fnp/conc tok_if
            expression
            tok_then
            statement))

(def case-stmnt
  (fnp/conc tok_case expression tok_of case-list tok_end))

(def while-stmnt
  (fnp/conc tok_while expression tok_do statement))

(def repeat-stmnt
  (fnp/semantics
   (fnp/conc tok_repeat statement-list tok_until expression)
   s/repeat-stmnt))

(def for-downto
  (fnp/semantics
   (fnp/conc expression
             tok_downto
             expression)
   s/for-downto))

(def for-to
  (fnp/semantics
   (fnp/conc expression
             tok_to
             expression)
   s/for-to))

(def for-list
  (fnp/alt
   for-downto
   for-to))

(def for-stmnt
  (fnp/semantics
   (fnp/conc tok_for varid op_assign for-list tok_do statement)
   s/for-stmnt))

(def procinvoke
  (fnp/semantics
   (fnp/conc procid
             (fnp/conc delim_lparen expression-list delim_rparen))
   s/procinvoke))

(def goto-stmnt
  (fnp/conc tok_goto label))

(def with-stmnt
  (fnp/conc tok_with record-variable-list tok_do statement))

(def label-stmnt
  (fnp/conc label delim_colon statement))

;;------------------------------------------------------------------------------
;; Statement v2.0

(def statement
  (fnp/alt
   statements
   assignment
   ifte
   ift
   case-stmnt
   while-stmnt
   repeat-stmnt
   for-stmnt
   procinvoke
   goto-stmnt
   with-stmnt
   label-stmnt
   fnp/emptiness))

;; Recursively defines valid variable or address access, being either a
;; raw symbol, a pointer deref or a miltipart indexing operation. Also
;; note that these can be nested arbitrarily, there is nothing wrong with
;; a statement such as
;; a[1, C, 5, 9001].bar[2]^ or soforth.
;; Note however that the `identifier` group is absoltuely terminal here.
;; As this will recur left until an identifier is found, we can devise
;; an equivalnet grammar by beginning with an identifier and recuring
;; right until we have reeached the limit of the tokens which this rule
;; can consume.

(def var-postfix
  (fnp/alt
   (fnp/conc delim_lbrack subscript-list delim_rbrack)
   (fnp/conc op_dot fieldid)
   (fnp/conc op_point)))

(def variable
  (fnp/semantics
   (fnp/conc identifier
             (fnp/rep* var-postfix))
   s/variable))

(def subscript-list
  (fnp/alt (fnp/conc expression delim_comma subscript-list)
           expression))

(def case-list
  (fnp/alt
   (fnp/conc statement delim_colon case-label-list delim_semi case-list)
   (fnp/conc statement delim_colon case-label-list)))

(def expression-list
  (fnp/semantics
   (fnp/conc expression
             (fnp/opt
              (fnp/conc delim_comma
                        expression-list)))
   s/tail-cons))

(def label
  unsigned-integer)

(def record-variable-list
  (fnp/alt variable
           (fnp/conc variable
                     delim_comma
                     record-variable-list)))
(def expression
  (fnp/semantics
   (fnp/conc additive-expression
             (fnp/opt
              (fnp/conc relational-op expression)))
   s/additive-expression))

(def relational-op
  (fnp/alt op_le
           op_ne
           op_lt
           op_ge
           op_eq
           op_gt))

(def additive-expression
  (fnp/semantics
   (fnp/conc multiplicative-expression
             (fnp/opt
              (fnp/conc additive-op
                        additive-expression)))
   s/additive-expression))

(def additive-op
  (fnp/alt op_add op_sub op_or))

(def multiplicative-expression
  (fnp/semantics
   (fnp/conc unary-expression
             (fnp/opt
              (fnp/conc multiplicative-op
                        multiplicative-expression)))
   s/additive-expression))

(def multiplicative-op
  (fnp/alt op_mul
           op_div
           op_mod
           op_and
           ))

(def unary-expression
  (fnp/semantics
   (fnp/conc (fnp/opt unary-op)
             primary-expression)
   s/unary-expression))

(def unary-op
  (fnp/alt op_add op_sub op_not))

(def primary-expression
  (fnp/alt procinvoke
           (fnp/conc delim_lbrack element-list delim_rbrack)
           (fnp/conc delim_lparen expression delim_rparen)
           variable
           unsigned-integer
           unsigned-real
           string
           tok_nil))

(def element-list
  (fnp/alt (fnp/conc element delim_comma element-list)
           element
           fnp/emptiness))

(def element
  (fnp/alt (fnp/conc expression delim_dotdot expression)
           expression))

(def constid identifier)
(def typeid identifier)
(def funcid identifier)
(def procid identifier)
(def fieldid identifier)
(def varid identifier)
