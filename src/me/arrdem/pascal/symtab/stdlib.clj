(ns me.arrdem.pascal.symtab.stdlib
  (:require [me.arrdem.compiler.symbols :refer [->FunctionType]]
            [me.arrdem.compiler.symtab :refer [install!]]))

(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table to install the basic Pascal functions."
  []
  ;; install the standard library
  (doseq [f [
             ["exp" #{'("real")} "real"]
             ["trexp" #{'("real")} "real"]
             ["sin" #{'("real")} "real"]
             ["cos" #{'("real")} "real"]
             ["trsin" #{'("real")} "real"]
             ["sqrt" #{'("real")} "real"]
             ["round" #{'("real")} "real"]
             ["iround" #{'("real")} "integer"]
             ["ord" #{'("char")} "integer"]
             ["trnew" #{'("integer")} "integer"]
             ["write" #{-1} nil]
             ["writeln" #{-1} nil]
             ["writef" #{'("real")} nil]
             ["writelnf" #{'("real")} nil]
             ["writei" #{'("integer")} nil]
             ["writelni" #{'("integer")} nil]
             ["read" #{} nil]
             ["readln" #{} nil]
             ["eof" #{} "boolean"]
             ["ctoi" #{'("char")} "integer"]
             ["btoi" #{'("boolean")} "integer"]
             ["itof" #{'("integer")} "real"]
             ]]
    (install! (apply ->FunctionType f))))
