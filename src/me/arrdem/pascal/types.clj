(ns me.arrdem.pascal.types
  (:require [clojure.set :as set]

            [me.arrdem.pascal.symtab :refer [search install!]]
            [me.arrdem.pascal.ast :refer [e-> makefuncall]]

            [loom.graph  :as graph]
            [loom.alg    :as traversals]))

;;------------------------------------------------------------------------------
;; Basic Pascal type tree as presented here
;;
;;            Real
;;              ^
;;              |
;;           integer
;;           ^     ^
;;          /       \
;;     character  boolean

(def -type-graph
  "A directed graph representing the basic state of Pascal's almost nonexistent
type conversion hierarchy."
  (-> (graph/digraph)
      (graph/add-edges ["integer"   "real"])
      (graph/add-edges ["boolean"   "integer"])
      (graph/add-edges ["character" "integer"])))

(def ^:dynamic *type-graph* (atom -type-graph))

;;------------------------------------------------------------------------------
;; Utilities
(defn ^:dynamic transformer-name
  "A rebindable function which computes "
  ([x y]
     (str (name x) "->" (name y))))

;;------------------------------------------------------------------------------
;; traversal & computation functions
(defn conversion-path
  "Resolves the type conversion sequence for going between two types. Returns
a pair of sequences, being the intermediate types (if any) which that argument
must be cast through to reach the minimum common representation. Eg. a
resolve-conversion for \"character\" and \"boolean\" should yield
((\"integer\") (\"integer\")) as the minimum common type is integer."
  ([type1 type2]
     (let [t1 (traversals/bf-traverse @*type-graph* type1)
           t2 (traversals/bf-traverse @*type-graph* type2)

           common (set/intersection (set t1)
                                    (set t2))

           candidates (map (juxt (partial traversals/shortest-path
                                          @*type-graph* type1)
                                 (partial traversals/shortest-path
                                          @*type-graph* type2))
                            common)
           shortest (first (sort (comparator
                            (fn [p1 p2] (<= (apply + (map count p1))
                                            (apply + (map count p2)))))
                       candidates))]
           (map rest shortest))))

(defn path->transformer
  "Transforms a conversion-path into a function f being a macro style functions
taking expressions as arguments and returning the appropriate type converted
expression. Depends on the type conversion resolution operations."
  ([path]
     (let [steps (map vector path (rest path))]
       #(apply e-> %1 (map (partial apply transformer-name)
                           steps)))))

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
