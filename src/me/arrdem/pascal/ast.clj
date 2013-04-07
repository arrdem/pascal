(ns ^{:doc    "A more structured setting for the various functions which are
               used to manipulate and generate abstract syntax tree elements.
               Intended for use as a more elegant backend to semantics, and
               as a utility suite for macros when they come down the pipe."
      :author "Reid McKenzie"
      :added  "0.2.0"}
  me.arrdem.pascal.ast
  (:require [me.arrdem.pascal.symtab :refer [search install! genlabel!]]))

;;------------------------------------------------------------------------------
;; Symbol table manipulation
(defn abs-name
  ([sym]
     (println "; searching for symbol" sym)
     (or (:qname sym)
         (:qname (search sym)))))

(defn dbg-install
  ([v]
     (println "; declared var " v)
     (install! v)
     (abs-name v)))

;;------------------------------------------------------------------------------
;; Expression manipulators
(defn ecomp [fx fn]
  `(~fn ~fx))

(defn e-> [v & fns]
  (reduce ecomp v fns))

;;------------------------------------------------------------------------------
;; Expression fragments
(defn makecomment [& cmnts]
  `("comment" ~@cmnts))

(defn makegoto [label]
  `("goto" ~label))

(defn makeprogn [forms]
  (if (< 1 (count forms))
    `("progn" ~@forms)
    (first forms)))

(defn binop [e0 op e1]
  `(~op ~e0 ~e1))

(defn makelabel [v]
  `("label" ~v))

(defn makeif
  ([test s] (makeif test s nil))
  ([test s e] `("if" ~test ~s ~e)))

(defn makederef [sym]
  (:qname (search sym)))

(defn makefuncall [sym args]
  `("funcall" ~(symbol sym) ~@args))

;;------------------------------------------------------------------------------
;; Common control structures
(defn makewhile [test s]
  (let [label (genlabel!)]
    (makeprogn
     [(makelabel label)
      (makeif test
              (makeprogn [s
                          (makegoto ~label)]))
      ])))

(defn makerepeat [s test]
  (let [label (genlabel!)]
    (makeprogn
      [(makelabel label)
      s
      (makeif (ecomp test "not")
              (makegoto label))])))
