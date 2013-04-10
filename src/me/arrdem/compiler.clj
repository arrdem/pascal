(ns ^{:doc "Defines the Compiler structure and some operations thereon which are
            used to backend the other structures in me.arrdem.compiler.*"
      :added "0.3.0"
      :author "Reid McKenzie"}
      me.arrdem.compiler
      (:require [me.arrdem.macros :refer [-<n>]]))
;;------------------------------------------------------------------------------
;; Define the Compiler record structure
;;
;; {:gensym    <gensym counter>
;;  :label     <label counter>
;;  :symtab    <symbol table map>
;;  :typegraph <tree of type conversions>
;;  :cur-ns    <the namespace list>
;;  :ast       <ast strucure>
;; }

(def get-ns :cur-ns)
(def get-symtab :symtab)
(def get-ast :ast)
(def get-typematrix :typegraph)
(def -get-gensym :gensym)
(def -get-label :label)

;;------------------------------------------------------------------------------
;; manipulate the namespace "stack"
(defn descend!
  "Pushes the argument namespace onto the ns stack, altering how symbols are
   resolved until the ns stack is popped. Used for recuring into function and
   program definitions which may have local bindings. Returns an updated
   compiler state record."
  [compiler ns]
  (update-in compiler [get-ns] concat (list ns)))

(defn ascend!
  "Pops the stack, taking any symbols defined in a nested ns out of scope.
   Invoked when returning from function and program definitions as they may
   contain symbol bindings. Returns an updated compiler state record."
  [compiler]
  (update-in compiler [get-ns] butlast))

;;------------------------------------------------------------------------------
;; Symbol generation tools
(defn reset-symns!
  "Nukes the stack value restoring it to its base state. Usefull for testing and
   for multiple compile runs without restart. Returns the updated compiler
   state record."
  [compiler]
  (assoc compiler get-ns (list)))

(defn ninc
  "An inc which doesn't friggin die on nil."
  [x]
  (if x (inc x) 0))

(defn key-inc-update [k m]
  [(get m key) (update-in m [key] ninc)])

;;------------------------------------------------------------------------------
;; Symbol generation
(defn gensym!
  "Generates a symbol name (string) which is guranteed by use of an
   incriminating counter to be unique to the current compile session. Optionally
   takes a string prefix for the generated name which does not effect the
   numeric part of the name. Returns a string being the prefix argument or
   \"G__\" followed by the string render of the gensym counter before it was
   incremented. Returns a pair (sym-name new-compiler-state)"
  ([compiler]
     (gensym! compiler "G__"))
  ([compiler prefix]
     (-<n> compiler
           (key-inc-update -get-gensym <>)
           [(str prefix <1>) <2>])))

(defn reset-gensym!
  "Nukes the gensym counter restoring it to its base state. Usefull for testing,
   multiple compile runs without restart. Returns the updated compiler state."
  [compiler]
  (assoc compiler -get-gensym 0))

;;------------------------------------------------------------------------------
;; Label generation
(defn genlabel!
  "Generates and returns an integer label, side-effecting the label count of
   compiler state & returning a pair (label-number new-compiler-state)."
  [compiler]
  (-<n> compiler
        (key-inc-update -get-label <>)
        [<1> <2>]))

(defn reset-genlabel!
  "Nukes the label generation counter & returns an updated compiler state."
  [compiler]
  (assoc compiler -get-label 0))
