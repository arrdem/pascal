(ns ^{:doc "The symbol table manipulation fragment of the compiler data
            structure. Defines utility functions for installing symbols
            and searching for symbols within the current scope as
            tracked by the namespace stack."
      :added "0.1.5"
      :author "Reid McKenzie"}
  me.arrdem.compiler.symtab
  (:require [clojure.string :refer [split]]
            [me.arrdem.compiler :refer [get-symtab get-ns]]
            [me.arrdem.macros :refer [-<n>]]))

;;------------------------------------------------------------------------------
;; Namespace stringification and reading
(defn- -render-ns
  "Renders the ns stack to a prefix string for symbols."
  ([stack]
     (if (< 1 (count stack))
       (str (apply str (interpose \. (butlast stack)))
            "/" (last stack))
       (first stack))))

(defn render-ns
  [compiler]
  (-render-ns (get-ns compiler)))

(defn decomp-ns
  "Unrenders a namespace"
  [name]
  (split name #"[\./]"))

;;------------------------------------------------------------------------------
;; Manipulation routines for the various symbol registries which this system
;; uses. Other namespaces define partials atop these meta-functions which are
;; then used to manipulate the macro system, the type system and soforth.
(defn install!
  "Symbol installer. Takes a compiler structure and a record as arguments,
   performs the appropriate symbol insertion update with respect to the
   namespace stack. Returns a pair (installed-qname new-compiler-state)."
  [compiler sym]
  (let [path (cons get-symtab
                   (get-ns compiler))
        qpath (concat (get-ns compiler) (list (:name sym)))]
    [(-render-ns qpath) (update-in compiler path assoc (:name sym) sym)]))

(defn- ns-search
  "Recursively searches the symbol table for a symbol with an argument name.
   Returns the symbol map more the :qname key if such a symbol exists or nil
   on search failure."
  ([symtab sym stack]
     (let [qualified-sym (concat stack (list sym))
           rstack        (rest stack)]
       (or (if-let [v (get-in get-symtab qualified-sym)]
             (assoc v :qname (-render-ns qualified-sym))
             (if (empty? stack) nil))
           (if-not (empty? stack)
             (recur symtab sym rstack))))))

(defn search
  "Searches the symbol table for a given named symbol, returning the symbol
   record more its absolute identifier as :qname or nil if there is no such
   symbol."
  [compiler name]
  (let [stack (decomp-ns name)
        name  (last stack)
        stack (butlast stack)
        stack (if (empty? stack)
                (get-ns compiler)
                stack)]
    (ns-search (get-symtab compiler) name stack)))
