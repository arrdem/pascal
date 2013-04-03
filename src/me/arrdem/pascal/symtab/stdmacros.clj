(ns me.arrdem.pascal.symtab.stdmacros
  (:require [me.arrdem.compiler.symtab :refer [install! search typeof sizeof]]
            [me.arrdem.pascal. :refer :all]))

(defn Macro [name fn]
  {:name     name
   :type     :macro
   :expander fn})

;;------------------------------------------------------------------------------

(defn p-new-macro
  "A macro function which serves to boostrap the equivalent of a malloc call.
Takes on argument: a type, and expands to a call to the trnew function which
actually allocates memory at runtime."
  ([[t]]
     (let [T (search t)]
       (assert (not (nil? T))
               (str "Failed to find type " t " in the symbol tbl"))
       `(~'funcall "trnew" ~(sizeof t)))))

;;------------------------------------------------------------------------------

(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table and install the standard macros used for pre-code generation type
ensuring and soforth."
  ([]
     (doseq [m [(Macro "new" p-new-macro)
                ]]
       (install! m))))
