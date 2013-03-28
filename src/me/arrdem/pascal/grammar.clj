(ns me.arrdem.pascal.grammar
  (:require [name.choi.joshua.fnparse :as fnp]
            [me.arrdem.sad.runtime :refer [defrule]]))

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
    label-declaration
    unary-expression
    subscript-list
    proc-or-func
    empty
    label
    pascal-program
    simple-type
    element-list
    for-list
    block5
    procid
    case-list
    block-or-forward
    element
    variable
    constant
    record-field
    expression
    fixed-part
    variable-declaration
    variableid-list
    case-label-list
    primary-expression
    fieldid
    identifier-list
    field-list
    block3
    parameterid-list
    relational-op
    block4
    block1
    block2
    funcid
    structured-type
    formal-parameter-list
    block
    formal-parameter-section
    statement-list
    variant-list
    type-declaration
    constid
    expression-list
    proc-and-func-declaration
    unary-op
    fieldid-list
    parameters
    varid
    typeid
    statement
    multiplicative-expression
    additive-op
    variant-part
    constant-declaration
    tag-field
    type
    record-variable-list
    program-heading
    additive-expression
    index-list
    variant
    multiplicative-op)

(defrule tok (fnp/lit ""))
(defrule tok_nil (fnp/lit "nil"))
(defrule tok_not (fnp/lit "not"))
(defrule tok_in (fnp/lit "in"))
(defrule tok_and (fnp/lit "and"))
(defrule tok_mod (fnp/lit "mod"))
(defrule tok_div (fnp/lit "div"))
(defrule tok_FSLASH (fnp/lit "/"))
(defrule tok_STAR (fnp/lit "*"))
(defrule tok_or (fnp/lit "or"))
(defrule tok_G (fnp/lit ">"))
(defrule tok_EQ_G (fnp/lit "=>"))
(defrule tok_L (fnp/lit "<"))
(defrule tok_L_G (fnp/lit "<>"))
(defrule tok_L_EQ (fnp/lit "<="))
(defrule tok_downto (fnp/lit "downto"))
(defrule tok_to (fnp/lit "to"))
(defrule tok_with (fnp/lit "with"))
(defrule tok_goto (fnp/lit "goto"))
(defrule tok_for (fnp/lit "for"))
(defrule tok_until (fnp/lit "until"))
(defrule tok_repeat (fnp/lit "repeat"))
(defrule tok_do (fnp/lit "do"))
(defrule tok_while (fnp/lit "while"))
(defrule tok_else (fnp/lit "else"))
(defrule tok_then (fnp/lit "then"))
(defrule tok_if (fnp/lit "if"))
(defrule tok_COL_EQ (fnp/lit ":="))
(defrule tok_function (fnp/lit "function"))
(defrule tok_procedure (fnp/lit "procedure"))
(defrule tok_case (fnp/lit "case"))
(defrule tok_packed (fnp/lit "packed"))
(defrule tok_file (fnp/lit "file"))
(defrule tok_set (fnp/lit "set"))
(defrule tok_record (fnp/lit "record"))
(defrule tok_of (fnp/lit "of"))
(defrule tok_RB (fnp/lit "]"))
(defrule tok_LB (fnp/lit "["))
(defrule tok_array (fnp/lit "array"))
(defrule tok_DOT_DOT (fnp/lit ".."))
(defrule tok_UP (fnp/lit "^"))
(defrule tok_SUB (fnp/lit "-"))
(defrule tok_P (fnp/lit "+"))
(defrule tok_COL (fnp/lit ":"))
(defrule tok_var (fnp/lit "var"))
(defrule tok_type (fnp/lit "type"))
(defrule tok_EQ (fnp/lit "="))
(defrule tok_const (fnp/lit "const"))
(defrule tok_label (fnp/lit "label"))
(defrule tok_end (fnp/lit "end"))
(defrule tok_begin (fnp/lit "begin"))
(defrule tok_COMMA (fnp/lit ","))
(defrule tok_RP (fnp/lit ")"))
(defrule tok_LP (fnp/lit "("))
(defrule tok_DOT (fnp/lit "."))
(defrule tok_SEMI (fnp/lit ";"))
(defrule tok_program (fnp/lit "program"))

(defrule pascal-program
  (fnp/conc tok_program
            identifier
            program-heading
            tok_SEMI
            block
            tok_DOT))

(defrule program-heading
  (fnp/conc tok_LP
            identifier-list
            tok_RP))

(defrule identifier-list
  (fnp/alt (fnp/conc identifier
                     tok_COMMA
                     identifier-list)
           identifier))

(defrule block
  (fnp/alt (fnp/conc label-declaration
                     tok_SEMI
                     block1)
           block1))

(defrule block1
  (fnp/alt (fnp/conc constant-declaration
                     tok_SEMI
                     block2)
           block2))

(defrule block2
  (fnp/alt (fnp/conc type-declaration
                     tok_SEMI
                     block3)
           block3))

(defrule block3
  (fnp/alt (fnp/conc variable-declaration
                     tok_SEMI
                     block4)
            block4))

(defrule block4
  (fnp/alt (fnp/conc proc-and-func-declaration
                     tok_SEMI
                     block5)
           block5))

(defrule block5
  (fnp/conc tok_begin
            statement-list
            tok_end))

(defrule label-declaration
  (fnp/conc tok_label
            unsigned-integer
            (fnp/rep*
             (fnp/conc
              tok_COMMA
              unsigned-integer))))

(defrule constant-declaration
  (fnp/alt
    (fnp/conc
      identifier
      tok_EQ
      constant
      tok_SEMI
      constant-declaration)
    (fnp/conc tok_const
              identifier
              tok_EQ
              constant)))

(defrule type-declaration
  (fnp/alt
    (fnp/conc identifier
              tok_EQ
              type
              tok_SEMI
              type-declaration)
    (fnp/conc tok_type
              identifier
              tok_EQ
              type)))

(defrule vardecl
  (fnp/conc variableid-list
            tok_COL
            type))

(defrule vardecls
  (fnp/alt (fnp/conc vardecl tok_semi vardecls)
           vardecl))

(defrule variable-declaration
  (fnp/conc tok_var
            vardecls))

(defrule variableid-list
  (fnp/alt (fnp/conc identifier
                     tok_COMMA
                     variableid-list)
           identifier))

(defrule constant
  (fnp/alt integer
           real
           string
           constid
           (fnp/conc tok_P constid)
           (fnp/conc tok_SUB constid)))

(defrule type
  (fnp/alt simple-type
           structured-type
           (fnp/conc tok_UP typeid)))

(defrule simple-type
  (fnp/alt
    (fnp/conc tok_LP
              identifier-list
              tok_RP)
    (fnp/conc constant
              tok_DOT_DOT
              constant)
    typeid))

(defrule structured-type
  (fnp/alt
    (fnp/conc tok_array tok_LB index-list tok_RB tok_of type)
    (fnp/conc tok_record field-list tok_end)
    (fnp/conc tok_set tok_of simple-type)
    (fnp/conc tok_file tok_of type)
    (fnp/conc tok_packed structured-type)))

(defrule index-list
  (fnp/alt (fnp/conc simple-type
                     tok_COMMA
                     index-list)
           simple-type))

(defrule field-list
  (fnp/alt
   (fnp/conc fixed-part tok_SEMI variant-part)
    fixed-part
    variant-part))

(defrule fixed-part
  (fnp/alt record-field
           (fnp/conc fixed-part
                     tok_SEMI
                     record-field)))

(defrule record-field
  (fnp/alt (fnp/conc fieldid-list
                     tok_COL
                     type)
            empty))

(defrule fieldid-list
  (fnp/alt (fnp/conc identifier tok_COMMA fieldid-list)
           identifier))

(defrule variant-part
  (fnp/conc tok_case
            tag-field
            tok_of
            variant-list))

(defrule tag-field
  (fnp/alt typeid
           (fnp/conc identifier
                     tok_COL
                     typeid)))

(defrule variant-list
  (fnp/alt variant
           (fnp/conc variant-list
                     tok_SEMI
                     variant)))

(defrule variant
  (fnp/alt
    (fnp/conc case-label-list
              tok_COL
              tok_LP
              field-list
              tok_RP)
    empty))

(defrule case-label-list
  (fnp/alt
   (fnp/conc constant
             tok_COMMA
             case-label-list)
   constant))

(defrule proc-and-func-declaration
  (fnp/alt proc-or-func
           (fnp/conc proc-or-func
                     tok_SEMI
                     proc-and-func-declaration)))

(defrule proc-or-func
  (fnp/alt
    (fnp/conc procedure
              identifier
              parameter
              sopt
              tok_SEMI
              block-or-forward)
    (fnp/conc function
              identifier
              parameters
              opt
              tok_COL
              typeid
              tok_SEMI
              block-or-forward)))

(defrule block-or-forward
  (fnp/alt block forward))

(defrule parameters
  (fnp/conc tok_LP
            formal-parameter-list
            tok_RP))

(defrule formal-parameter-list
  (fnp/alt (fnp/conc formal-parameter-section
                     tok_SEMI
                     formal-parameter-list)
           formal-parameter-section))

(defrule formal-parameter-section
  (fnp/alt
    (fnp/conc parameterid-list tok_COL typeid)
    (fnp/conc tok_var parameterid-list tok_COL typeid)
    (fnp/conc tok_procedure identifier parametersopt)
    (fnp/conc tok_function identifier parametersopt tok_COL typeid)))

(defrule parameterid-list
  (fnp/alt (fnp/conc identifier tok_COMMA parameterid-list)
           identifier))

(defrule statement-list
  (fnp/alt (fnp/conc statement tok_SEMI statement-list)
           statement))

(defrule statement
  (fnp/alt
    (fnp/conc variable tok_COL_EQ expression)
    (fnp/conc tok_begin statement-list tok_end)
    (fnp/conc tok_if expression tok_then statement)
    (fnp/conc tok_if expression tok_then statement tok_else statement)
    (fnp/conc tok_case expression tok_of case-list tok_end)
    (fnp/conc tok_while expression tok_do statement)
    (fnp/conc tok_repeat statement-list tok_until expression)
    (fnp/conc tok_for varid tok_COL_EQ for-list tok_do statement)
    procid
    (fnp/conc procid tok_LP expression-list tok_RP)
    (fnp/conc tok_goto label)
    (fnp/conc tok_with record-variable-list tok_do statement)
    (fnp/conc label tok_COL statement)
    empty))

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

(defrule var-postfix
  (fnp/alt
   (fnp/conc variable tok_LB subscript-list tok_RB)
   (fnp/conc variable tok_DOT fieldid)
   (fnp/conc variable tok_UP)))

(defrule var-postfixes
  (fnp/alt (fnp/conc var-postfix var-postfixes)
           var-postfix))

(defrule variable
  (fnp/alt
   (fnp/conc identifier var-postfixes)
   identifier))

(defrule subscript-list
  (fnp/alt (fnp/conc expression tok_COMMA subscript-list)
           expression))

(defrule case-list
  (fnp/alt
    (fnp/conc statement tok_COL case-label-list tok_SEMI case-list)
    (fnp/conc statement tok_COL case-label-list)))

(defrule for-list
  (fnp/alt
    (fnp/conc expression tok_to expression)
    (fnp/conc expression tok_downto expression)))

(defrule expression-list
  (fnp/alt expression
           (fnp/conc expression-list
                     tok_COMMA
                     expression)))

(defrule label
  unsigned-integer)

(defrule record-variable-list
  (fnp/alt variable
           (fnp/conc variable
                     tok_COMMA
                     record-variable-list)))
(defrule expression
  (fnp/alt
    (fnp/conc expression relational-op additive-expression)
    additive-expression))

(defrule relational-op
  (fnp/alt tok_L_EQ
           tok_L_G
           tok_L
           tok_EQ_G
           tok_EQ tok_G))

(defrule additive-expression
  (fnp/alt
    (fnp/conc multiplicative-expression
              additive-op
              additive-expression)
    multiplicative-expression))

(defrule additive-op
  (fnp/alt tok_P tok_SUB tok_or))

(defrule multiplicative-expression
  (fnp/alt
    (fnp/conc unary-expression
              multiplicative-op
              multiplicative-expression)
    unary-expression))

(defrule multiplicative-op
  (fnp/alt tok_STAR
           tok_FSLASH
           tok_div
           tok_mod
           tok_and
           tok_in))

(defrule unary-expression
  (fnp/alt (fnp/conc unary-op unary-expression)
           primary-expression))

(defrule unary-op
  (fnp/alt tok_P tok_SUB tok_not))

(defrule primary-expression
  (fnp/alt variable
           unsigned-integer
           unsigned-real
           string
           tok_nil
           (fnp/conc funcid
                     tok_LP expression-list tok_RP)
           (fnp/conc tok_LB element-list tok_RB)
           (fnp/conc tok_LP expression tok_RP)))

(defrule element-list
  (fnp/alt (fnp/conc element tok_COMMA element-list)
           element
           empty))

(defrule element
  (fnp/alt (fnp/conc expression tok_DOT_DOT expression)
           expression))

(defrule constid identifier)
(defrule typeid identifier)
(defrule funcid identifier)
(defrule procid identifier)
(defrule fieldid identifier)
(defrule varid identifier)
(defrule empty tok)
