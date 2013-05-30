(ns me.arrdem.pascal.symtab.stdtypes
  (:require (me.arrdem.compiler.symbols
             [core :refer [->PrimitiveType ->PointerType]]
             [complex :refer [->VariableType]])
            [me.arrdem.compiler.symtab :refer [search install!]]))

;; documentation has moved, see compiler.symbols for details on the
;; representation of vars, pointers and soforth in the symbol table.
(defn init!
  "Function of no arguments which serves simply to populate the symbol table
   with the standard 'primitive' types which Pascal supports."
  []
  (doseq [t [;; Basic types
             ["integer" 8]
             ["char" 1]
             ["boolean" 8]
             ["real" 8]
             ]]
    (install! (apply ->PrimitiveType t)))

  (doseq [t [;; Pointer types
             ["^integer" 8 "integer"]
             ["^char" 8 "char"]
             ["^boolean" 8 "boolean"]
             ["^real" 8 "real"]
             ]]
    (install! (apply ->PointerType t)))

  ;; create a nil type and a nil variable
  (let [niltype (->PrimitiveType "^niltype" 0)]
    (install! niltype)
    (install! (->VariableType "nil" niltype 0))))
