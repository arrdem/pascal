(ns ^{:doc "A more structured setting for the various functions which are used
            to manipulate and generate abstract syntax tree elements. Intended
            for use as a more elegant backend to semantics, and as a utility
            suite for macros."
      :author "Reid McKenzie"
      :added  "0.2.0"}
  me.arrdem.compiler.ast)

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
(defn binnot [form]
  `(~'not form))

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
