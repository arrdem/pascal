(ns me.arrdem.pascal.symtab.stdlib
  (:require [me.arrdem.compiler.symbols :refer [->FunctionType ->VariableType
                                                ->PrimitiveType]]
            [me.arrdem.compiler.symtab :refer [install!]]))

(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table to install the basic Pascal functions."
  []
  (println "; installing standard library...")
  ;; create a nil type and a nil variable
  (let [niltype (->PrimitiveType "niltype" 0)]
    (install! niltype)
    (install! (->VariableType "nil" niltype 0)))

  ;; install the standard library
  (doseq [f [
             ["exp" #{"real"} "real"]
             ["trexp" #{"real"} "real"]
             ["sin" #{"real"} "real"]
             ["cos" #{"real"} "real"]
             ["trsin" #{"real"} "real"]
             ["sqrt" #{"real"} "real"]
             ["round" #{"real"} "real"]
             ["iround" #{"real"} "integer"]
             ["ord" #{"char"} "integer"]
             ["trnew" #{"integer"} "integer"]
             ["write" #{"char"} nil]
             ["writeln" #{"char"} nil]
             ["writef" #{"real"} nil]
             ["writelnf" #{"real"} nil]
             ["writei" #{"integer"} nil]
             ["writelni" #{"integer"} nil]
             ["read" #{} nil]
             ["readln" #{} nil]
             ["eof" #{} "boolean"]
             ["ctoi" #{"char"} "integer"]
             ["btoi" #{"boolean"} "integer"]
             ["itof" #{"integer"} "real"]
             ]]
    (install! (apply ->FunctionType f)))
  (println "; standard library installed!"))
