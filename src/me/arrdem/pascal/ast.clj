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
;     (println "; searching for symbol" sym)
     (:qname
      (cond
       (map? sym)    (if (contains? sym :qname)
                       (select-keys sym [:qname])
                       (search (:name sym)))
       (string? sym) (search sym)))))

(defn dbg-install
  ([v]
;     (println "; declared var " v)
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
  `(~'comment ~@cmnts))

(defn makegoto [label]
  `(~'goto ~label))

(defn makeprogn-v [forms]
  `(~'progn ~@forms))

(defn makeprogn [forms]
  (if (< 1 (count forms))
    (makeprogn-v forms)
    (first forms)))

(defn binop [e0 op e1]
  `(~op ~e0 ~e1))

(defn makelabel [v]
  `(~'label ~v))

(defn makeif
  ([test s] `(~'if ~test ~s))
  ([test s e] (if e `(~'if ~test ~s ~e)
                  (makeif test s))))

(defn makederef [sym]
  (:qname (search sym)))

(defn makefuncall [sym args]
  `(~'funcall ~sym ~@args))

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
      (makeif (ecomp test 'not)
              (makegoto label))])))
