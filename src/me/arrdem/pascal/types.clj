(ns ^{:doc "A namespace defining type manipulation operations such as adding,
            sizing and indexing record types as well as computing AST
            structures for type casting."
      :added "0.2.1"
      :author "Reid McKenzie"}
      me.arrdem.pascal.types
  (:require [me.arrdem.compiler.type-hierarchy :as h]
            [me.arrdem.compiler.symtab :refer [search install!]]
            [me.arrdem.pascal.ast :refer [e-> makefuncall]]
            [loom.graph  :as graph]))

(def -type-graph
  "A directed graph representing the basic state of Pascal's almost nonexistent
type conversion hierarchy. The graph is of the form
        Real
         ^
      integer
      ^     ^
character  Boolean
to be exact"
  (-> (graph/digraph)
      (graph/add-edges ["integer"   "real"])
      (graph/add-edges ["boolean"   "integer"])
      (graph/add-edges ["character" "integer"])))

(def *type-graph* (atom -type-graph))

;;------------------------------------------------------------------------------
;; Public api for computing the minimum representation of types and type casts.
;; Note that it's pretty tied up in my symbol table architecture and naming
;; scheme, hence it's implementation in a pascal. namespace rather than in
;; the compiler.
(defn ^:dynamic transformer-name
  "A way to look up the macro which computes a given type transform."
  ([x y]
     (if y
       (str (name x) "->" (name y))
       (name x))))

(defn- path->transformer
  "Transforms a conversion-path into a function f being a macro style functions
taking expressions as arguments and returning the appropriate type converted
expression. Depends on the type conversion resolution operations."
  ([path]
     (let [steps (map vector path
                             (rest path))]
       #(apply e-> %1 (map (partial apply transformer-name)
                     steps)))))

(defn convert
  "Special case of a type conversion for forcing an expression to a known type.
Intended for use when assigning an int to a float variable and soforth."
  ([typed-expr from to]
     ((path->transformer (first (h/conversion-path @*type-graph* from to)))
      typed-expr)))

(defn level
  "Named because it computes the \"level\" representation type for the two
expressions, this function transforms both expression arguments to the type of
the minimum common representation according to the type graph."
  ([e0 t0 e1 t1]
     (let [[c0 c1] (h/conversion-path @*type-graph* t0 t1)]
       [((path->transformer c0) e0)
        ((path->transformer c1) e1)])))

;;------------------------------------------------------------------------------
;; Type matrix manipulation expressions
(defmacro with-types
  ([binding & forms]
     `(binding [*type-graph* ~binding]
        ~@forms)))

(defn install-transformer!
  ([from to entry]
     (swap! *type-graph* graph/add-edges [from to])
     (install! (assoc entry
                 :name (transformer-name from to)))))
