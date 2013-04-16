(ns me.arrdem.compiler.symtab
  (:require [clojure.string :refer [split]]
            [me.arrdem.compiler.symbols :refer [nameof]]))

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

(def ^:dynamic *symns*
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
  [ns] (swap! *symns* concat (list ns)))

(defn ascend!
  "Pops the *synms* stack, taking any symbols defined in a nested ns out of
scope. Invoked when returning from function and program definitions as they may
contain symbol bindings."
  [] (swap! *symns* butlast))

(defn reset-symns!
  "Nukes the *symns* value restoring it to its base state. Usefull for testing,
multiple compile runs without restart."
  ([] (reset! *symns* (list))))

;;------------------------------------------------------------------------------
;; The symbol table

(def ^:dynamic *symtab*
  "Used to track all symbols."
  (atom {}))

(defn ninc
  "An inc which doesn't friggin die on nil."
  [x] (if x (inc x) 0))

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
                    update-in [:gensym] ninc)))))

(defn reset-gensym!
  "Nukes the *symtab* gensym counter restoring it to its base state. Usefull for
testing, multiple compile runs without restart."
  ([] (swap! *symtab* assoc :gensym 0)))


(defn genlabel!
  "Generates and returns an integer label, side-effecting the :label count of the
*symtab* registry."
  ([] (:label
       (swap! *symtab*
              update-in [:label] ninc))))

(defn reset-genlabel!
  "Nukes the *symtab* genlabel counter restoring it to its base state. Usefull for
testing, multiple compile runs without restart."
  ([] (swap! *symtab* assoc :label 0)))

;;------------------------------------------------------------------------------
;; Namespace stringification and destringification

(defn render-ns
  "Renders the *symns* stack to a prefix string for symbols."
  ([] (render-ns @*symns*))
  ([stack]
     (if (< 1 (count stack))
       (str (apply str (interpose \. (butlast stack)))
            "/" (last stack))
       (first stack))))

(defn decomp-ns
  "Unrenders a namespace"
  [name]
  (split name #"[\./]"))

;;------------------------------------------------------------------------------
;; Manipulation routines for the various symbol registries which this system
;; uses. Other namespaces define partials atop these meta-functions which are
;; then used to manipulate the macro system, the type system and soforth.

(defn m-install!
  "Meta symbol installer. Takes a namespace structure and a record as arguments,
and performs the appropriate swap! respecting the namespacing stack."
  [atom sym]
  (let [path (concat @*symns* (list (nameof sym)))
        sym (assoc sym :qname (render-ns path))]
    (swap! atom assoc-in path sym)
    sym))

(defn- stack-search
  "Recursively searches the symbol table for a symbol with an argument name.
Returns the symbol map if such a symbol exists. Failure behavior is undefined,
but the returning a nil value and throwing an exception are both acceptable."
  ([atom sym stack]
     (let [qualified-sym (concat stack (list sym))
           rstack        (rest stack)]
       (or (if-let [v (get-in @atom qualified-sym)]
             v (when (empty? stack) nil))
           (when-not (empty? stack)
             (recur atom sym rstack))))))

(defn m-search
  "A wrapper around stack-search wich provides the base case logic required to
parse fully qualified names into a full stack path. Defaults to using
stack-search before returning a failure result."
  [atom name]
  (let [stack (decomp-ns name)
        name  (last stack)
        stack (butlast stack)
        stack (if (empty? stack) @*symns* stack)]
    (stack-search atom name stack)))

;;------------------------------------------------------------------------------
;; The public symbol table searching routines

(def search (partial m-search *symtab*))
(def install! (partial m-install! *symtab*))
