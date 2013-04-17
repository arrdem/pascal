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
  (extends? MacroType obj))

(defn pmacroexpand
  "An \"outermost first\" macro implementation. Looks up macros from the symbol
table, and applies them if possible. Note that in the two arguments case, the
second argument is the key used for pulling transformation functions out of
symbol table entries. This exists so that the macro system can be employed first
 at AST generation time to do type conversion and soforth, and later at code
generation time so that I'm not writing two macro systems when one will do."
  [expr]
  (if (seq? expr)
    (let [expander (pmacroexpand (first expr) key)
          expander (if (symbol? expander)
                     (name expander)
                     (str expander))
          expander (search expander)
          expander (if (macro? expander)
                     (.expander expander)
                     identity)
          res      (if expander
                     (apply expander (rest partial))
                     (cons (first expr) (next partial)))
          res      (cons (first res) (map #(pmacroexpand %1 key)
                                          (rest res)))]
      (if expander
        (pmacroexpand res key)
        res))
    expr))
