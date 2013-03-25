(ns me.arrdem.pascal.tokens
  (:require [lexington.lexer       :refer :all]
            [lexington.utils.lexer :refer :all]
            [me.arrdem.pascal.util :refer [deftoken make-lexer string]]))

(make-lexer pascal-base
;;------------------------------------------------------------------------------
;; Comments & ignored crap
  :pcomment      #"\(\*.*\*\)"
  :bcomment      #"\{.*\}"
  :ws            #"[ \t\r\n]"

;;------------------------------------------------------------------------------
;; Literal tokens
  (deftoken tok_array     "array")
  (deftoken tok_downto    "downto")
  (deftoken tok_function  "function")
  (deftoken tok_of        "of")
  (deftoken tok_repeat    "repeat")
  (deftoken tok_until     "until")
  (deftoken tok_begin     "begin")
  (deftoken tok_else      "else")
  (deftoken tok_goto      "goto")
  (deftoken tok_packed    "packed")
  (deftoken tok_set       "set")
  (deftoken tok_var       "var")
  (deftoken tok_case      "case")
  (deftoken tok_end       "end")
  (deftoken tok_if        "if")
  (deftoken tok_procedure "procedure")
  (deftoken tok_then      "then")
  (deftoken tok_while     "while")
  (deftoken tok_const     "const")
  (deftoken tok_file      "file")
  (deftoken tok_label     "label")
  (deftoken tok_program   "program")
  (deftoken tok_to        "to")
  (deftoken tok_with      "with")
  (deftoken tok_do        "do")
  (deftoken tok_for       "for")
  (deftoken tok_nil       "nil")
  (deftoken tok_record    "record")
  (deftoken tok_type      "type")

;;------------------------------------------------------------------------------
;; Literal operators
  (deftoken op_add        "+"   :val)
  (deftoken op_sub        "-"   :val)
  (deftoken op_or         "or"  :val)
  (deftoken op_mul        "*"   :val)
  (deftoken op_mod        "mod" :val)
  (deftoken op_div        "/"   :val)
  (deftoken op_and        "and" :val)
  (deftoken op_assign     ":="  :val)
  (deftoken op_eq         "="   :val)
  (deftoken op_ne         "<>")
  (deftoken op_lt         "<"   :val)
  (deftoken op_le         "<="  :val)
  (deftoken op_ge         ">="  :val)
  (deftoken op_gt         ">"   :val)
  (deftoken op_point      "^"   :val)
  (deftoken op_dot        ".")

;;------------------------------------------------------------------------------
;; Literal deliminators
  (deftoken delim_comma   ",")
  (deftoken delim_semi    ";")
  (deftoken delim_colon   ":")
  (deftoken delim_lparen  "(")
  (deftoken delim_rparen  ")")
  (deftoken delim_lbrack  "[")
  (deftoken delim_rbrack  "]")
  (deftoken delim_dotdot  "..")

;;------------------------------------------------------------------------------
;; Data literals
  (deftoken pstring    string :val)
  (deftoken identifier #"[a-zA-Z][a-zA-Z0-9]*" :val)
  (deftoken floatnum   #"(([0-9]+)(e[\+\-]?[0-9]+))|(([0-9]+)\.([0-9]+)(e[\+\-]?[0-9]+)?)" :val)
  (deftoken intnum     #"[0-9]+" :val)
  )
