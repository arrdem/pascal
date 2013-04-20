(ns ^{:doc "A namespace defining type manipulation operations such as adding,
            sizing and indexing record types as well as computing AST
            structures for type casting."
      :added "0.2.1"
      :author "Reid McKenzie"}
      me.arrdem.pascal.types
  (:require [me.arrdem.compiler :refer [typeof]]
            [me.arrdem.compiler.symtab :refer [search install!]]
            [me.arrdem.compiler.type-hierarchy :as h]
            [me.arrdem.compiler.symbol-conversions]
            [me.arrdem.pascal.ast :refer [e->]]
            [loom.graph :as graph]))

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

(def ^:dynamic *type-graph* (atom -type-graph))

;;------------------------------------------------------------------------------
;; Public api for computing the minimum representation of types and type casts.
;; Note that it's pretty tied up in my symbol table architecture and naming
;; scheme, hence it's implementation in a pascal. namespace rather than in
;; the compiler.
(defn ^:dynamic transformer-name
  "A way to look up the macro which computes a given type transform."
  [x y]
  (-> (if y (str (name x) "->" (name y))
          (name x))
      (symbol)))

(defn- path->transformer
  "Transforms a conversion-path into a function f being a macro style functions
   taking expressions as arguments and returning the appropriate type converted
   expression. Depends on the type conversion resolution operations."
  [path]
  #(apply e-> %1
         (map (partial apply transformer-name)
              (map vector path
                   (rest path)))))

(defn convert
  "Special case of a type conversion for forcing an expression to a known type.
   Intended for use when assigning an int to a float variable and so forth.
   returns a new expression being typed-expr"
  [typed-expr from to]
  (if-not (= from to)
    (-> @*type-graph*
        (h/conversion-path from to)
        ((fn [x] (or (first x)
                     (reverse (second x)))))
        path->transformer
        (apply typed-expr '()))
    (if (or (string? typed-expr)
            (number? typed-expr))
      typed-expr
      (with-meta
        typed-expr
        {:type from}))))

(defn level
  "Named because it computes the \"level\" representation type for the two
   expressions, this function transforms both expression arguments to the type
   of the minimum common representation according to the type graph. Note that
   this function will _only_ convert to higher types"
  [& exprs]
  (if-not (apply = exprs)
    (map (fn [x y]
           ((path->transformer x) y))
         (->> exprs
              (map typeof)
              (apply h/conversion-path @*type-graph*))
         exprs)
    exprs))

;;------------------------------------------------------------------------------
;; Type matrix manipulation expressions
(defmacro with-types
  [binding & forms]
  `(binding [*type-graph* ~binding]
     ~@forms))

(defn install-transformer!
  [from to entry]
  (swap! *type-graph* graph/add-edges [from to])
  (install! (assoc entry
              :name (transformer-name from to))))
