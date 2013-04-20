(ns ^{:doc "A more structured setting for the various functions which are used
            to manipulate and generate abstract syntax tree elements. Intended
            for use as a more elegant backend to semantics, and as a utility
            suite for macros."
      :author "Reid McKenzie"
      :added  "0.2.0"}
  me.arrdem.pascal.ast
  (:require [me.arrdem.compiler :refer [typeof nameof]]
            [me.arrdem.compiler.symtab :refer [search install! genlabel!]]
            [me.arrdem.pascal.types :refer [convert level]]))

;;------------------------------------------------------------------------------
;; Symbol table manipulation

;; TODO: find uses of this guy and remove em
(defn ^:depricated abs-name
  "Performs a symbol table lookup for the argument symbol. As of the new string
   & protocol based type system this code is not needed yet it remains."
  [sym]
  ;; (println @me.arrdem.compiler.symtab/*symtab*)
  ;; (println "; searching for symbol" sym)
  (:qname
   (cond
    (map? sym)
    (if (contains? sym :qname)
      (select-keys sym [:qname])
      (search (:name sym)))
    (string? sym)
    (search sym))))

(defn dbg-install
  "Wrapper around install! which may provide pre-installation debug printing"
  [v]
  ;; (println "; declared var " v)
  (:qname (install! v)))

;;------------------------------------------------------------------------------
;; Expression manipulators
(defn ecomp
  "Composes to Expressions ergo 'ecomp'."
  [fx fn]
  `(~fn ~fx))

;; TODO: After removing e-> rename to e->
(defn er->
  "A reduce based implementation of the -> operator for ast components. As with
   clojure.core/-> this routine takes a single 'base' value and an arbitrary
   number of body forms and threads the base value through all the argument
   forms. Ex. (e-> 4 (+ 4) (/ 2)) => (/ (+ 4 4) 2). Used to nest expressions,
   especially partially computed expressions as in variable indexing."
  [val & forms]
  (reduce (fn [e form]
            (if (list? form)
             (concat (list (first form) e)
                     (rest form))
             (list form e)))
          val forms))

;; TODO: find uses of this guy and remove em
(defn ^:depricated e->
  "A 'traditional' recursive implementation of the -> operator, ported directly
   from clojure.core for use on AST groups."
  ([x] x)
  ([x form]
     (if (seq? form)
       `(~(first form) ~x ~@(next form))
       (list form x)))
  ([x form & more]
     (apply (partial e-> (e-> x form)) more)))

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

(defn binop
  "Computes a typed arithmetic expression for two arguments and an operator.
   Serves as a portal through which all arithmetic must pass and thus provides
   almost all required type conversion silently. In the three argument case the
   type of the resulting expression is undefined but will be the minimum common
   representation of the types of the argument expressions. In the four argument
   case the second expression will be coerced to the type of the first."
  ([e0 op e1]
     (let [lvlval (level e0 e1)]
       (with-meta
         `(~op ~@lvlval)
         {:type (->> lvlval
                    (map typeof)
                    (remove nil?)
                    first
                    typeof
                    nameof)})))
  ([e0 op e1 _]
     `(~op ~e0 ~(convert e1
                         (nameof (typeof e1))
                         (nameof (typeof e0))))))

(defn makelabel [v]
  `(~'label ~v))

(defn makeif
  ([test s]
     `(~'if ~test ~s))
  ([test s e]
     (if e `(~'if ~test ~s ~e)
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
