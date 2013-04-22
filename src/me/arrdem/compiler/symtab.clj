(ns me.arrdem.compiler.symtab
  (:require [clojure.string :refer [split]]
            [me.arrdem.compiler :refer [nameof]]
            [me.arrdem.compiler.namespace :refer :all]))

(defn- ninc
  "An inc which doesn't friggin die on nil."
  [x]
  (-> x
      (or 0)
      inc))

;;------------------------------------------------------------------------------
;; The symbol table

(def ^:dynamic *symtab*
  "Used to track all symbols."
  (atom {}))

(defn gensym
  "Generates a symbol name (string) which is guranteed by use of an
   incriminating counter to be unique to the current compile session. Optionally
   takes a string prefix for the generated name which does not effect the
   numeric part of the name. Returns a string being the prefix argument or
   \"G__\" followed by the string render of the gensym counter before it was
   incremented."
  ([table] (gensym! table "G__"))
  ([table s] (-> table
                 (update-in table [:gensym] ninc)
                 :gensym
                 ((partial str s)))))

(defn reset-gensym
  "Nukes the *symtab* gensym counter restoring it to its base state. Usefull for
   testing, multiple compile runs without restart."
  [table]
  (assoc table :gensym 0))

(defn genlabel
  "Generates and returns an integer label, side-effecting the :label count of the
   *symtab* registry."
  [table]
  (-> table
      (update-in [:label] ninc)
      (:label)))

(defn reset-genlabel
  "Nukes the *symtab* genlabel counter restoring it to its base state. Usefull for
   testing, multiple compile runs without restart."
  [table]
  (assoc table :label 0))

;;------------------------------------------------------------------------------
;; Manipulation routines for the various symbol registries which this system
;; uses. Other namespaces define partials atop these meta-functions which are
;; then used to manipulate the macro system, the type system and soforth.

(defn symbol-install
  "Meta symbol installer. Takes a namespace structure and a record as arguments,
   and performs the appropriate swap! respecting the namespacing stack."
  [table ns-stack sym]
  (let [path (concat ns-stack (list (nameof sym)))
        sym (assoc sym :qname (render-ns path))]
    (assoc-in table path sym)
    sym))

(defn search
  "Recursively searches the symbol table for a symbol with an argument name.
   Returns the symbol map if such a symbol exists. Failure behavior is
   undefined, but the returning a nil value and throwing an exception are both
   acceptable."
  [table ns-stack sym]
  (let [qualified-sym (concat ns-stack (list sym))
        rstack (rest stack)]
    (or (if-let [v (get-in table qualified-sym)]
          v (when (empty? stack) nil))
        (when-not (empty? stack)
          (recur table sym rstack)))))

;;------------------------------------------------------------------------------
;; side-effectful wrappers around the above stateless API
(defmacro with-symtab
  ""
  [table & body]
  `(binding [*symtab* table]
    ~@body))

(defn set-symtab!
  ""
  [table]
  (alter-var-root #'*symtab*
                  (constantly table)))

(defn symtab!
  ""
  [& fns]
  (set-symtab! {})
  (doseq [f fns]
    (f)))
