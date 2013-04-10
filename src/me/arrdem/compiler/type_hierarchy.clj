(ns ^{:doc "A graph representation of a type hierarchy usable for resolving the
            minimum representation for the product of two typed objects."
      :added "0.2.1"
      :author "Reid McKenzie"}
  me.arrdem.compiler.type-hierarchy
  (:require [clojure.set :as set]
            [me.arrdem.macros :refer [-<n>]]
            [me.arrdem.compiler :refer [get-typematrix]]
            [me.arrdem.compiler.symtab :refer [install! search]]
            [loom.graph :as graph]
            [loom.alg :as traversals]))

;;------------------------------------------------------------------------------
;; traversal & computation functions
(defn conversion-path
  "Resolves the type conversion sequence for going between two types. Returns
   a pair of sequences, being the intermediate types (if any) which that
   argument must be cast through to reach the minimum common representation. Eg.
   a resolve-conversion for \"character\" and \"boolean\" should yield
   '((\"integer\") (\"integer\")) as the minimum common type is integer. Note
   that this algorithm assumes that the type tree is a directed graph with no
   cycles where any node M reachable from node N  _fully represents_ the data
   stored by the type of node N."
  [graph type1 type2]
  (-<n> [type1 type2]
        (map (partial traversals/bf-traverse graph) <>)
        (map set <>)
        (apply set/intersection <>)
        (map (apply juxt (map #(partial traversals/shortest-path graph %1)
                              [type1 type2]))
             <>)
        (first (sort (comparator
                      (fn [& ps]
                        (apply <=
                               (map
                                (comp
                                 (partial apply +)
                                 count) ps))))
                     <>))))

(defn install-transformer!
  "Installs a type transformation function both in the symbol table and in the
   compiler's type translation graph. Note that this function depends upon the
   entry argument featuring argument and return type data. Returns the updated"
  [compiler entry]
  (let [path (apply vector
                    (map (comp :qname search)
                         ((juxt :type/arg :type/ret) entry)))]
    (assert (every? (comp not nil?) path))
    (assert (not (contains? (apply set (graph/edges (get-typematrix compiler)))
                            path)))
    (-<n> compiler
     (graph/add-edges <1> path)
     (install! <> entry))))
