(ns me.arrdem.compiler.macros
  (:require [me.arrdem.compiler.symbols :refer [typeof]]
            [me.arrdem.compiler.symtab :refer [search]]))

(defrecord MacroType [name expander]
  me.arrdem.compiler.symbols.ISymbol
    (typeof [self] "macro")
    (nameof [self] (.name self))
    (sizeof [self] nil)
    (addrof [self] nil))

(defn macro? [obj]
  (try
    (= "macro" (typeof obj))
    (catch Exception e false)))

(declare pmacroexpand)

(defn _pmacroexpand
  "An \"outermost first\" macro implementation. Looks up macros from the symbol
   table, and applies them if possible."
  [expr]
  (println "; macroexpand got param of type " (type expr))
  (if (seq? expr)
    (do (println "; macroexpanding " expr)
        (let [expander (pmacroexpand (first expr))
              expandfn (cond
                        (macro? expander) expander
                        (string? expander) (search expander)
                        (symbol? expander) (search (name expander))
                        true nil)
              expandfn (when (macro? expandfn)
                         (.expander expandfn))
              res (if (fn? expandfn)
                    (expandfn (apply list (rest expr)))
                    expr)]
          (println "; macroexpanding intermediate state " res)
          (let [res (cons (first res)
                          (doall (map pmacroexpand (apply list (rest res)))))]
            (if (and (fn? expandfn) ;; possibility of recursive macro
                     (not (= res expr))) ;; prevent non-transforming recursion
              (pmacroexpand res)
              res))))
    expr))

(def pmacroexpand (memoize _pmacroexpand))
