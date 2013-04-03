(ns me.arrdem.compiler.macros
  (:require [me.arrdem.compiler.types :refer [typeof]]
            [me.arrdem.compiler.symtab :refer [search]]))

;;------------------------------------------------------------------------------
;; Predefined macros
;; ("functions") which need to be expanded via some sort of macro system in
;; order to properly express their runtime behavior: ex. new() which requires
;; type data.
;;
;; MACROS:
;;   Macros shall have the type :macro, and the key :expander. As with lisp
;;   macros, macros will be invoked with arguments equal to the tail of the base
;;   form and are expected to return either an atom, or a sequence which will be
;;   assumed contain macros and will be macroexpanded until such time as the
;;   returned value is no longer a list, or is empty, or the expand is equal to
;;   the input. Also, macros shall only be defined at the top level and for this
;;   pascal compiler are not user-definable. Hardcoded only. Other language
;;   implementations may specify macros, but that is beyond the scope of this
;;   design although trivially implemented.

(defn Macro
  "Wrapper function equivalent to (deftype Macro) which makes initialization of
Macro records quick and relatively easy."
  [name fn]
  {:name     name
   :type     :macro
   :expander fn})

(defn macro?
  "Predicate to test whether a resolved symbol is a macro or not. Tests the
:type key, asserting that it is :macro."
  [sym]
  (= :macro (typeof sym)))

(def expander
  "Operator to get the expander of a macro record."
  :expander)

(defn pmacroexpand
  "An \"outermost first\" macro implementation. Looks up macros from the symbol
table, and applies them if possible. Note that in the two arguments case, the
second argument is the key used for pulling transformation functions out of
symbol table entries. This exists so that the macro system can be employed first
 at AST generation time to do type conversion and soforth, and later at code
generation time so that I'm not writing two macro systems when one will do."
  ([expr]
     (pmacroexpand expr expander))
  ([expr key]
     (if (seq? expr)
       (let [expander (pmacroexpand (first expr) key)
             expander (if (symbol? expander)
                        (name expander)
                        (str expander))
             expander (search expander)
             expander (if (macro? expander)
                        (get expander key identity))
             res      (if expander
                        (apply expander (rest partial))
                        (cons (first expr) (next partial)))
             res      (cons (first res) (map #(pmacroexpand %1 key)
                                             (rest res)))]
         (if expander
           (pmacroexpand res key)
           res))
       expr)))
