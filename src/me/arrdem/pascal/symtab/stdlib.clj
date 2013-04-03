(ns me.arrdem.pascal.symtab.stdlib
  (:require [me.arrdem.compiler.symtab :refer [install!]]))

(defn Function [name ret-type & arg-types]
  {:name name
   :type :fn
   :type/ret ret-type
   :type/arg arg-types})

(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table to install the basic Pascal functions."
  ([]
     (doseq [f [(Function "exp"      "real"    "real")
                (Function "trexp"    "real"    "real")
                (Function "sin"      "real"    "real")
                (Function "cos"      "real"    "real")
                (Function "trsin"    "real"    "real")
                (Function "sqrt"     "real"    "real")
                (Function "round"    "real"    "real")
                (Function "iround"   "integer" "real")
                (Function "ord"      "integer" "char")
                (Function "new"      "integer" "integer") ;; this guy needs to be a macro
                (Function "trnew"    "integer" "integer")
                (Function "write"    nil       "char")
                (Function "writeln"  nil       "charsym")
                (Function "writef"   nil       "real")
                (Function "writelnf" nil       "real")
                (Function "writei"   nil       "integer")
                (Function "writelni" nil       "integer")
                (Function "read"     nil       )
                (Function "readln"   nil       )
                (Function "eof"      "boolean" )
                  ]]
       (install! f))))
