(ns me.arrdem.pascal.hooks
  (:require [name.choi.joshua.fnparse :as fnp]
            [me.arrdem.pascal.debug :as dbg]))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defmacro defrule
  ([sym form] `(defrule ~sym "" ~form))
  ([sym docstring form]
     `(def ~sym ~docstring
        (fnp/semantics
         (fnp/conc
          (fnp/effects
           (me.arrdem.pascal.debug/push ~(name sym))
           ((get-pre-hook (quote ~sym))))
          (fnp/semantics ~form
                     (get-semantics (quote ~sym)))
          (fnp/effects
           ((get-post-hook (quote ~sym)))
           (me.arrdem.pascal.debug/scope-pop)))
         second))))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(defn nothing [] nil)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Side-effect "hook" system

(def bnf-hooks-registry (atom {}))

(defn set-prefixed-hook [prefix sym fn]
  (swap! bnf-hooks-registry
         assoc (symbol prefix (name sym))
         (if (instance? clojure.lang.IFn fn)
           fn (eval fn))))

(def set-post-hook (partial set-prefixed-hook "post"))
(def set-pre-hook  (partial set-prefixed-hook "pre"))

(defn get-prefixed-hook
  "Searches the prefix table for a prefixed symbol hook, returning the hook
if one is registered otherwise returning the function Nothing."
  [prefix sym]
  (or ((symbol prefix (name sym))
       @bnf-hooks-registry) nothing))

(def get-post-hook
  "Wrapper on get-prefixed-hook which instructs it to look for a \"post\" hook."
  (partial get-prefixed-hook "post"))

(def get-pre-hook
  "Wrapper on get-prefixed-hook which instructs it to look for a \"pre\" hook."
  (partial get-prefixed-hook "pre"))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; Semantics manipulation system

(def bnf-semantics-registry (atom {}))

(defn get-semantics [sym]
  "Searches the semantics table for a transform function, returning
clojure.core/identity if there is no registered transform."
  (or (sym @bnf-semantics-registry) identity))

(defn set-semantics
  "Relates the function or expression fn to the symbol sym in the semantics
table, evaluating the expression with the expectation of receiving a function
if the fn argument does not already implement clojure.lang.IFn."
  [sym fn]
  (if (instance? clojure.lang.IFn fn)
    (swap! bnf-semantics-registry assoc sym fn)
    (swap! bnf-semantics-registry assoc sym (eval fn))))
