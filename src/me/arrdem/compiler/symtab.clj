(ns me.arrdem.compiler.symtab
  (:require [clojure.string :refer [split]]
            [me.arrdem.compiler.symtab-util :refer [m-install! m-search]]))

(def base_st
  {
;;------------------------------------------------------------------------------
;; Values
;; These are "magic" values which various parts of the compiler rely on.

   :label  0 ;; counter used for label generation
   :gensym 0 ;; counter shared by all symbol generation

   })

;;------------------------------------------------------------------------------
;; The namespace stack

(def ^:dynamic ^:private *symns*
  "Used to track the namespace levels above the current point of evaluation.
An empty list signifies that we are operating at the \"top\" level where program
forms and other such values live. It is here that the \"standard library\" lives.
When decending to another namespace the decend! function is called and a value is
pushed onto the head of this list. When returning from a nested namespace, the
rise! function is called which pops the top element off of this stack.

Symbol resolution is performed by iteratively prefixing the symbol to be resolved
with the concatonation of the stack, searching and poping until either the symbol
is resolved, or the stack is empty."
  (atom '()))

(defn descend!
  "Pushes the argument namespace onto the *symns* stack, altering how symbols are
resolved until the *symns* stack is poped. Used for recuring into function
and program definitions which may have local bindings."
  [ns] (swap! *symns* conj ns))

(defn ascend!
  "Pops the *synms* stack, taking any symbols defined in a nested ns out of
scope. Invoked when returning from function and program definitions as they may
contain symbol bindings."
  [] (swap! *symns* pop))

;;------------------------------------------------------------------------------
;; The symbol table

(def ^:dynamic ^:private *symtab*
  "Used to track all symbols."
  (atom {}))

(defn gensym!
  "Generates a symbol name (string) which is guranteed by use of an incrementing
counter to be unique to the current compile session. Optionally takes a string
prefix for the generated name which does not effect the numeric part of the
name. Returns a string being the prefix argument or \"G__\" followed by the
string render of the gensym counter before it was incremented."
  ([] (gensym! "G__"))
  ([s] (str s
            (:gensym
             (swap! *symtab*
                    update-in [:gensym] inc)))))

(defn genlabel!
  "Generates and returns an integer label, side-effecting the :label count of the
*symtab* registry."
  ([] (:label
       (swap! *symtab*
              update-in [:label] inc))))

(defn render-ns
  "Renders the *symns* stack to a prefix string for symbols."
  ([] (render-ns @*symns*))
  ([stack]
     (if (< 1 (count stack))
       (str (apply str (interpose \. (reverse (next stack))))
            "/" (first stack))
       (first stack))))

(defn decomp-ns
  "Unrenders a namespace"
  [name]
  (reverse (split name #"[\./]")))

;;------------------------------------------------------------------------------
;; The public symbol table searching routines

(def search (partial m-search *symns*))
(def install! (partial m-install! *symns*))
