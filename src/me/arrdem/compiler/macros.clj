(ns me.arrdem.compiler.macros
  (:require [me.arrdem.compiler :refer [typeof]]
            [me.arrdem.compiler.symtab :refer [search]]))

(defrecord MacroType [name expander]
  me.arrdem.compiler.ISymbol
    (typeof [self] "macro")
    (nameof [self] (.name self))
    (sizeof [self] nil)
    (addrof [self] nil))

(defn macro? [obj]
  (try
    (= "macro" (typeof obj))
    (catch Exception e false)))

(defn macro-val [token]
  (cond
   (string? token) (search token)
   (symbol? token) (macro-val (name token))
   (macro? token) token
   true nil))

(defn pmacroexpand
  "An \"outermost first\" macro implementation. Looks up macros from the symbol
   table, and applies them if possible."
  [expr]
  ;; (println "; macroexpand got param of type " (type expr))
  (if (seq? expr)
    (do ;; (println "; macroexpanding " expr)
        (let [expander (pmacroexpand (first expr))
              expandfn (macro-val expander)
              expandfn (when (macro? expandfn)
                         (.expander expandfn))
              res (if (fn? expandfn)
                    (expandfn (apply list (rest expr)))
                    expr)]
          ;; (println "; macroexpanding intermediate state " res)
          (let [res (if (seq? res)
                      (apply list
                             (cons (first res)
                                   (doall (map pmacroexpand (apply list (rest res))))))
                      res)]
            (if (and (fn? expandfn) ;; possibility of recursive macro
                     (not (= res expr))  ;; prevent non-transforming recursion
                     (seq? res)) ;; there's something left to expand
              (pmacroexpand res)
              res))))
    expr))
