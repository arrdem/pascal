(ns me.arrdem.pascal.util
  (:require [clojure.string        :as string ]
            [lexington.lexer       :refer :all]
            [lexington.utils.lexer :refer :all]
            [name.choi.joshua.fnparse :as fnp]))

(defmacro make-lexer
  "As I use Lexington a common design pattern is to define a lexer using keyed
regular expressions within Lexington, and then to define a series of token
predicate rules which fnparse can interact with separately. This leads to
massive duplication of effectively equivalent code as I first define a table of
regex to symbol translations and then define an equivalent series of symbol to
predicate translations.

This macro in conjunction with deftoken is designed to close that loop by in one
expression both generating the Lexington deflexer expression and interning the
requisite predicates for manipulating Lexington's output from fnparse.

This macro assumes that args is an ordered mix of either pairs (symbol, re) or
traditional Clojure map pairs of key : re in the order of the order of the seq."
  [sym & args] `(deflexer ~sym ~@(-> args
                                     (#(map macroexpand %))
                                     flatten)))

(defmacro deftoken
  "Generates an fnparse predicate bound to the symbolic argument, and returns
a pair (key pattern) which the make-lexer macro can format into place for a
Lexington lexer."
  ([symbol pattern]
     (let [k (keyword (name symbol))]
       (eval `(def ~symbol
                (fnp/semantics
                 (fnp/term
                  #(= (:lexington.tokens/type %1) ~k))
                 :val)))
       (list k pattern))))

;;------------------------------------------------------------------------------
;; Common regex patterns

(def whitespace    #" |\t|\r|\n")
(def simple-string #"\"[^\"]+\"")
(def string        #"(\"((\"\")|[^\"])*\")|('(('')|[^'])*')")
(def lisp-comment  #";+.*[\n\r]+")
(def word          #"[a-zA-Z\-]+")

;;------------------------------------------------------------------------------
;; Helper functions

(defn wordfn
  "Simply joins the read characters into a string."
  ([v] ((comp (partial apply str)
              :lexington.tokens/data)
        v)))

(defn strfn
  "A simple transform for a delimited character sequence which drops the
delimiters and generates the string contents. Does not account for or
attempt to reduce out escaped characters."
  ([v] (let [s (apply str (:lexington.tokens/data v))
             str-delim (str (first s))]
         (-> s
             (string/replace (-> str-delim
                                 ((partial repeat 2))
                                 ((partial apply str))
                                 (re-pattern))
                             str-delim)
             butlast
             (#(drop 1 %1))
             (#(apply str %1))))))

(def readerfn
  "A function for processing Lexington tokens which applies the Clojure reader
to the read string. Intended for processing literals such as numbers, symbols
and keywords into their Clojure representation."
  (comp read-string
        #(apply str %)
        :lexington.tokens/data))
