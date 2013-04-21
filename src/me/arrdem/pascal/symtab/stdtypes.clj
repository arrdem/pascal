(ns me.arrdem.pascal.symtab.stdtypes
  (:require [me.arrdem.compiler.symbols :refer [->PrimitiveType ->PointerType
                                                ->VariableType]]
            [me.arrdem.compiler.symtab :refer [search install!]]))

;; documentation has moved, see compiler.symbols for details on the
;; representation of vars, pointers and soforth in the symbol table.
(defn init!
  "Function of no arguments which serves simply to populate the symbol table
   with the standard 'primitive' types which Pascal supports."
  []
  (println "; installing standard types...")
  (doseq [t [;; Basic types
             ["integer" 4]
             ["char" 1]
             ["boolean" 4]
             ["real" 8]
             ]]
    (install! (apply ->PrimitiveType t)))

  (doseq [t [;; Pointer types
             ["^integer" 4 "integer"]
             ["^char" 4 "char"]
             ["^boolean" 4 "boolean"]
             ["^real" 4 "real"]
             ]]
    (install! (apply ->PointerType t)))

  ;; create a nil type and a nil variable
  (let [niltype (->PrimitiveType "^niltype" 0)]
    (install! niltype)
    (install! (->VariableType "nil" niltype 0)))

  (println "; standard types installed!"))
