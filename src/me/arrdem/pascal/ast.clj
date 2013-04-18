(ns ^{:doc    "A more structured setting for the various functions which are
               used to manipulate and generate abstract syntax tree elements.
               Intended for use as a more elegant backend to semantics, and
               as a utility suite for macros when they come down the pipe."
      :author "Reid McKenzie"
      :added  "0.2.0"}
  me.arrdem.pascal.ast
  (:require [me.arrdem.compiler.symtab :refer [search install! genlabel!]]))

;;------------------------------------------------------------------------------
;; Symbol table manipulation
(defn abs-name
  ([sym]
     ;; (println @me.arrdem.compiler.symtab/*symtab*)
    ;; (println "; searching for symbol" sym)
     (:qname
      (cond
       (map? sym)    (if (contains? sym :qname)
                       (select-keys sym [:qname])
                       (search (:name sym)))
       (string? sym) (search sym)))))

(defn dbg-install
  ([v]
    ;; (println "; declared var " v)
     (:qname (install! v))))

;;------------------------------------------------------------------------------
;; Expression manipulators
(defn ecomp [fx fn]
  `(~fn ~fx))

(defn er-> [val & forms]
  (reduce (fn [e form]
            (if (list? form)
             (concat (list (first form) e)
                     (rest form))
             (list form e)))
          val forms))

(defn e->
  ([x] x)
  ([x form] (if (seq? form)
              `(~(first form) ~x ~@(next form))
              (list form x)))
  ([x form & more] (apply (partial e-> (e-> x form)) more)))

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

(defn partial-make-aref [index]
  `(~'aref ~index))

;;------------------------------------------------------------------------------
;; Common control structures
(defn makewhile [test s]
  (let [label (genlabel!)]
    (makeprogn
     [(makelabel label)
      (makeif test
              (makeprogn [s
                          (makegoto label)]))
      ])))

(defn makerepeat [s test]
  (let [label (genlabel!)]
    (makeprogn
      [(makelabel label)
      s
      (makeif (ecomp test 'not)
              (makegoto label))])))
