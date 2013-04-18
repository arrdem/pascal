(ns ^{:doc "A graph representation of a type hierarchy usable for resolving the
            minimum representation for the product of two typed objects."
      :added "0.2.1"
      :author "Reid McKenzie"}
  me.arrdem.compiler.type-hierarchy
  (:require [clojure.set :as set]
            [loom.alg    :as traversals]))

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
  ([graph & types]
     (let [traversals (map (partial traversals/bf-traverse graph)
                           types)
           common (apply set/intersection
                         (map set traversals))

           candidates (map (apply juxt
                                  (map #(partial traversals/shortest-path graph %1)
                                       types))
                            common)
           shortest (first
                     (sort
                      (comparator
                       (fn [p1 p2] (<= (apply + (map count p1))
                                       (apply + (map count p2)))))
                      candidates))]
           shortest)))
