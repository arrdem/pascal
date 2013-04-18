(ns me.arrdem.pascal.symtab.stdmacros
  (:require [me.arrdem.compiler.symtab :refer [install! search]]
            [me.arrdem.compiler.symbols :refer [sizeof nameof]]
            [me.arrdem.compiler.macros :refer [->MacroType]]
            [me.arrdem.pascal.ast :refer [makefuncall binop]]))

;;------------------------------------------------------------------------------

(defn p-new-macro
  "A macro function which serves to boostrap the equivalent of a malloc call.
Takes on argument: a type, and expands to a call to the trnew function which
actually allocates memory at runtime."
  [[t]]
  (let [T (search t)]
    (assert (not (nil? T)) (str "Failed to find type " t " in the symbol tbl"))
    (assert (not (string? T)) (str "got a string for " t " in the symbol tbl"))
    (binop t ':= (makefuncall "trnew" (list (sizeof T))))))

(defn- progn? [form]
  (and (list? form)
       (= (first form) 'progn)))

(defn progn-inliner
  "A macro function which serves to try and inline out nested Mprogn groups.
   Derived from
   https://github.com/valeryz/MacroPHP/blob/master/special-forms.lisp#L20"
  [body]
  (if (list? body)
    (reduce (fn [prev form]
              (concat prev
                      (if (progn? form)
                        (progn-inliner (rest form))
                        (list form))))
            nil body)
    body))

;;------------------------------------------------------------------------------

(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table and install the standard macros used for pre-code generation type
ensuring and soforth."
  []
  (doseq [m [["new" p-new-macro]
             ["progn" progn-inliner]
             ]]
    (install! (apply ->MacroType m))))
