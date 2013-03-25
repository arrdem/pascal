(ns me.arrdem.pascal.lexer
    (:require [lexington.lexer       :refer :all]
              [lexington.utils.lexer :refer :all]
              [me.arrdem.pascal.util :refer :all]))

(make-lexer pascal-base
;;------------------------------------------------------------------------------
;; Comments & ignored crap
  :pcomment      #"\(\*.*\*\)"
  :bcomment      #"\{.*\}"
  :ws            #"[ \t\r\n]"

;;------------------------------------------------------------------------------
;; Data literals
  (deftoken pstring  string)
  (deftoken floatnum #"(([0-9]+)(e[\+\-]?[0-9]+))|(([0-9]+)\.([0-9]+)(e[\+\-]?[0-9]+)?)")
  (deftoken intnum   #"[0-9]+")

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
  (deftoken op_add        "+")
  (deftoken op_sub        "-")
  (deftoken op_mul        "*")
  (deftoken op_div        "_")
  (deftoken op_assign     ":=")
  (deftoken op_eq         "=")
  (deftoken op_ne         "<>")
  (deftoken op_lt         "<")
  (deftoken op_le         "<=")
  (deftoken op_ge         ">=")
  (deftoken op_gt         ">")
  (deftoken op_point      "^")
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
  (deftoken identifier    #"[a-zA-Z]+")
  )

(def pascal
  (-> pascal-base
      (discard :ws)
      (discard :pcomment)
      (discard :bcomment)

      (generate-for :identifier :val readerfn)
      (generate-for :pstring    :val strfn)
      (generate-for :intnum     :val readerfn)
      (generate-for :floatnum   :val readerfn)
      ))
