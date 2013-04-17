(ns me.arrdem.pascal.symtab.stdmacros
  (:require [me.arrdem.compiler.symtab :refer [install! search]]
            [me.arrdem.compiler.symbols :refer [sizeof nameof]]
            [me.arrdem.compiler.macros :refer [->MacroType]]
            [me.arrdem.pascal.ast :refer [makefuncall]]))

;;------------------------------------------------------------------------------

(defn p-new-macro
  "A macro function which serves to boostrap the equivalent of a malloc call.
Takes on argument: a type, and expands to a call to the trnew function which
actually allocates memory at runtime."
  [[t]]
  (let [T (search t)]
    (println "[p-new-macro] found type T:" (nameof T))
    (assert (not (nil? T))
            (str "Failed to find type " t " in the symbol tbl"))
    (makefuncall "trnew" (list (sizeof T)))))

;;------------------------------------------------------------------------------

(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table and install the standard macros used for pre-code generation type
ensuring and soforth."
  []
  (doseq [m [["new" p-new-macro]
             ]]
    (install! (apply ->MacroType m))))
