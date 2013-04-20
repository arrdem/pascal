(ns me.arrdem.pascal.symtab.stdmacros
  (:require [me.arrdem.compiler :refer [sizeof nameof]]
            [me.arrdem.compiler.symtab :refer [install! search]]
            [me.arrdem.compiler.macros :refer [->MacroType]]
            [me.arrdem.pascal.ast :refer [makefuncall]]))

;;------------------------------------------------------------------------------

(defn p-new-macro
  "A macro function which serves to boostrap the equivalent of a malloc call.
Takes on argument: a type, and expands to a call to the trnew function which
actually allocates memory at runtime."
  [[t]]
  (let [T (search t)]
    (assert (not (nil? T)) (str "Failed to find type " t " in the symbol tbl"))
    (assert (not (string? T)) (str "got a string for " t " in the symbol tbl"))
    (list ':= t (makefuncall "trnew" (list (sizeof T))))))

;;------------------------------------------------------------------------------
(defn- progn? [form]
  (and (list? form)
       (= (first form) 'progn)))

(defn- _progn-inliner
  [body]
  (reduce (fn [prev form]
            (concat prev
                    (if (progn? form)
                      (_progn-inliner (rest form))
                      (list form))))
          nil body))

(defn progn-inliner
  "A macro function which serves to try and inline out nested progn groups.
   Derived from
   https://github.com/valeryz/MacroPHP/blob/master/special-forms.lisp#L20"
  [body]
  (if (list? body)
    (cons 'progn (_progn-inliner body))
    body))

;;------------------------------------------------------------------------------

;; TODO: arithmetic expression compressing routines

;; TODO: nested aref elimination

;;------------------------------------------------------------------------------
(defn init!
  "Function of no arguments, its sole purpose is to side-effect the symbol
table and install the standard macros used for pre-code generation type
ensuring and soforth."
  []
  (println "; installing standard macros...")
  (doseq [m [["new" p-new-macro]
             ["progn" progn-inliner]
             ]]
    (install! (apply ->MacroType m)))
  (println "; standard macros installed!"))
