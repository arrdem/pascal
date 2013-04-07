(ns ^{:doc "A graph representation of a type hierarchy usable for resolving the
            minimum representation for the product of two typed objects."
      :added "0.2.1"
      :author "Reid McKenzie"}
  me.arrdem.compiler.type-hierarchy
  (:require [clojure.set :as set]
            [loom.graph  :as graph]
            [loom.alg    :as traversals]))

(def ^:dynamic *type-graph* (atom -type-graph))

;;------------------------------------------------------------------------------
;; traversal & computation functions
(defn conversion-path
  "Resolves the type conversion sequence for going between two types. Returns
a pair of sequences, being the intermediate types (if any) which that argument
must be cast through to reach the minimum common representation. Eg. a
resolve-conversion for \"character\" and \"boolean\" should yield
'((\"integer\") (\"integer\")) as the minimum common type is integer. Note that
this algorithm assumes that the type tree is a directed graph with no cycles
where any node M reachable from node N  _fully represents_ the data stored by
the type of node N."
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
           shortest)))

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
