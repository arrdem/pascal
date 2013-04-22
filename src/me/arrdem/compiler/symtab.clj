(ns me.arrdem.compiler.symtab
  (:require [me.arrdem.compiler :refer [nameof]]
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
  (atom {}))

(defn gensym
  "Generates a symbol name (string) which is guranteed by use of an
   incriminating counter to be unique to the current compile session. Optionally
   takes a string prefix for the generated name which does not effect the
   numeric part of the name. Returns a string being the prefix argument or
   \"G__\" followed by the string render of the gensym counter before it was
   incremented."
  ([table] (gensym table "G__"))
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

(defn symbol-install
  "Meta symbol installer. Takes a namespace structure and a record as arguments,
   and performs the appropriate swap! respecting the namespacing stack."
  [table ns-stack sym]
  (let [path (concat ns-stack (list (nameof sym)))
        sym (assoc sym :qname (render-ns path))]
    (assoc-in table path sym)))

(defn __search-symtab
  "Recursively searches the symbol table for a symbol with an argument name.
   Returns the symbol map if such a symbol exists. Failure behavior is
   undefined, but the returning a nil value and throwing an exception are both
   acceptable."
  [table ns-stack sym]
  (let [qualified-sym (concat ns-stack (list sym))
        rstack (rest ns-stack)]
    (println "; [search-symtab]" table)
    (println "; [search-symtab] -" qualified-sym)

    (or (get-in table qualified-sym)
        (if (empty? ns-stack)
          nil
          (recur table sym rstack)))))

(defn search-symtab
  "A wrapper around stack-search wich provides the base case logic required to
parse fully qualified names into a full stack path. Defaults to using
stack-search before returning a failure result."
  [map name]
  (let [stack (decomp-ns name)
        name (last stack)
        stack (butlast stack)
        stack (if (empty? stack) (get-ns) stack)]
    (__search-symtab map stack name)))

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; side-effectful wrappers around the above stateless API
(defmacro with-symtab
  [table & body]
  `(binding [*symtab* (atom ~table)]
    ~@body))

(defn set-symtab!
  [table]
  (alter-var-root #'*symtab*
                  (constantly (atom table))))

(defn symtab!
  [& fns]
  (with-symtab
    (set-symtab! {})
    (doseq [f fns]
      (f))))

(defn search [symbol]
  (search-symtab @*symtab* symbol))

(defn install! [symbol]
  (swap! *symtab* symbol-install (get-ns) symbol)
  (search (nameof symbol)))
