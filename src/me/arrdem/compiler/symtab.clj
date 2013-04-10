(ns me.arrdem.compiler.symtab
  (:require [clojure.string :refer [split]]
            [me.arrdem.macros :refer [-<n>]]))

;;------------------------------------------------------------------------------
;; The namespace stack

(defn descend!
  "Pushes the argument namespace onto the *symns* stack, altering how symbols are
resolved until the *symns* stack is poped. Used for recuring into function
and program definitions which may have local bindings."
  [compiler ns]
  (update-in compiler [:cur-ns] concat (list ns)))

(defn ascend!
  "Pops the stack, taking any symbols defined in a nested ns out of scope.
Invoked when returning from function and program definitions as they may contain
 symbol bindings."
  [compiler]
  (update-in compiler [:cur-ns] butlast))

(defn reset-symns!
  "Nukes the stack value restoring it to its base state. Usefull for testing and
for multiple compile runs without restart."
  [compiler]
  (assoc :cur-ns (list)))

;;------------------------------------------------------------------------------
;; The symbol table

(defn ninc
  "An inc which doesn't friggin die on nil."
  [x]
  (if x (inc x) 0))

(defn key-inc-update [k m]
  [(get m key) (update-in m [key] ninc)])

(defn gensym!
  "Generates a symbol name (string) which is guranteed by use of an incrementing
counter to be unique to the current compile session. Optionally takes a string
prefix for the generated name which does not effect the numeric part of the
name. Returns a string being the prefix argument or \"G__\" followed by the
string render of the gensym counter before it was incremented."
  ([compiler]
     (gensym! compiler "G__"))
  ([compiler prefix]
     (-<n> compiler
           (key-inc-update :gensym <>)
           [(str prefix <1>) <2>])))

(defn reset-gensym!
  "Nukes the gensym counter restoring it to its base state. Usefull for testing,
 multiple compile runs without restart."
  [compiler]
  (assoc compiler :gensym 0))

(defn genlabel!
  "Generates and returns an integer label, side-effecting the :label count of
compiler state pseudo-object."
  [compiler]
  (-<n> compiler
        (key-inc-update :label <>)
        [(str prefix <1>) <2>]))

(defn reset-genlabel!
  "Nukes the *symtab* genlabel counter restoring it to its base state. Usefull
for testing, multiple compile runs without restart."
  [compiler]
  (assoc compiler :label 0))

;;------------------------------------------------------------------------------
;; Namespace stringification and destringification

(defn- -render-ns
  "Renders the ns stack to a prefix string for symbols."
  ([stack]
     (if (< 1 (count stack))
       (str (apply str (interpose \. (butlast stack)))
            "/" (last stack))
       (first stack))))

(defn render-ns
  [compiler]
  (-render-ns (:cur-ns compiler)))

(defn decomp-ns
  "Unrenders a namespace"
  [name]
  (split name #"[\./]"))

;;------------------------------------------------------------------------------
;; Manipulation routines for the various symbol registries which this system
;; uses. Other namespaces define partials atop these meta-functions which are
;; then used to manipulate the macro system, the type system and soforth.

(defn m-install!
  "Symbol installer. Takes a compiler structure and a record as arguments, and
performs the appropriate update with respect to the namespacing stack."
  [compiler sym]
  (let [path (conj :symtab (:cur-ns compiler))]
    (update-in compiler path assoc (:name sym) sym)))

(defn- stack-search
  "Recursively searches the symbol table for a symbol with an argument name.
Returns the symbol map if such a symbol exists. Failure behavior is undefined,
but the returning a nil value and throwing an exception are both acceptable."
  ([atom sym stack]
     (let [qualified-sym (concat stack (list sym))
           rstack        (rest stack)]
       (or (if-let [v (get-in @atom qualified-sym)]
             (assoc v :qname (render-ns qualified-sym))
             (if (empty? stack) nil))
           (if-not (empty? stack)
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
