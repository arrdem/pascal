(ns me.arrdem.compiler.symtab-util
  (:require [me.arrdem.compiler.symtab :refer [*symns* render-ns decomp-ns]]))

;;------------------------------------------------------------------------------
;; Manipulation routines for the various symbol registries which this system
;; uses. Other namespaces define partials atop these meta-functions which are
;; then used to manipulate the macro system, the type system and soforth.

(defn m-install!
  "Meta symbol installer. Takes a namespace structure and a record as arguments,
and performs the appropriate swap! respecting the namespacing stack."
  [atom sym]
  (let [path (conj @*symns* (:name sym))]
    (swap! atom assoc path sym)))

(defn- stack-search
  "Recursively searches the symbol table for a symbol with an argument name.
Returns the symbol map if such a symbol exists. Failure behavior is undefined,
but the returning a nil value and throwing an exception are both acceptable."
  ([atom sym stack]
     (let [qualified-sym (conj stack sym)
           rstack        (pop stack)]
       (or (if-let [v (get @atom qualified-sym)]
             (assoc v :qname (render-ns qualified-sym))
             (if (empty? stack) nil))
           (if-not (empty? stack)
             (recur atom sym rstack))))))

(defn m-search
  "A wrapper around stack-search wich provides the base case logic required to
parse fully qualified names into a full stack path. Defaults to using
stack-search before returning a failure result."
  [atom name]
  (let [stack (decomp-ns name)]
    (get @atom stack
         (stack-search atom name))))
