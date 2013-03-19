(ns me.arrdem.pascal.lexer
    (:require [lexington.lexer       :refer :all]
              [lexington.utils.lexer :refer :all]))

(deflexer pascal-base
;;------------------------------------------------------------------------------
;; Comments & ignored crap
  :pcomment      #"\(\*.*\*\)"
  :bcomment      #"\{.*\}"
  :ws            #"[ \t\r\n]"

;;------------------------------------------------------------------------------
;; Data literals
  :string        #"(\"[^\"]+\")|('[^']+')"
  :floatnum      #"(([0-9]+)(e[\+\-]?[0-9]+))|(([0-9]+)\.([0-9]+)(e[\+\-]?[0-9]+)?)"
  :intnum        #"[0-9]+"

;;------------------------------------------------------------------------------
;; Literal tokens
  :tok/array     "array"
  :tok/downto    "downto"
  :tok/function  "function"
  :tok/of        "of"
  :tok/repeat    "repeat"
  :tok/until     "until"
  :tok/begin     "begin"
  :tok/else      "else"
  :tok/goto      "goto"
  :tok/packed    "packed"
  :tok/set       "set"
  :tok/var       "var"
  :tok/case      "case"
  :tok/end       "end"
  :tok/if        "if"
  :tok/procedure "procedure"
  :tok/then      "then"
  :tok/while     "while"
  :tok/const     "const"
  :tok/file      "file"
  :tok/label     "label"
  :tok/program   "program"
  :tok/to        "to"
  :tok/with      "with"
  :tok/do        "do"
  :tok/for       "for"
  :tok/nil       "nil"
  :tok/record    "record"
  :tok/type      "type"

;;------------------------------------------------------------------------------
;; Literal operators
  :op/add        "+"
  :op/sub        "-"
  :op/mul        "*"
  :op/div        "/"
  :op/assign     ":="
  :op/eq         "="
  :op/ne         "<>"
  :op/lt         "<"
  :op/le         "<="
  :op/ge         ">="
  :op/gt         ">"
  :op/point      "^"
  :op/dot        "."

;;------------------------------------------------------------------------------
;; Literal deliminators
  :delim/comma   ","
  :delim/semi    ";"
  :delim/colon   ":"
  :delim/lparen  "("
  :delim/rparen  ")"
  :delim/lbrack  "["
  :delim/rbrack  "]"
  :delim/dotdot  ".."

;;------------------------------------------------------------------------------
  :identifier    #"[a-zA-Z]+"
  )

(def strfn
  (fn [v] (apply str (drop 1 (butlast (:lexington.tokens/data v))))))

(def reader
  (comp read-string #(apply str %1) :lexington.tokens/data))

(def pascal
  (-> pascal-base
      (discard :ws)
      (discard :pcomment)
      (discard :bcomment)

      (generate-for :identifier :val reader)
      (generate-for :string     :val strfn)
      (generate-for :intnum     :val reader)
      (generate-for :floatnum   :val reader)
      ))
