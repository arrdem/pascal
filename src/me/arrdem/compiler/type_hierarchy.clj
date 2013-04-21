(ns ^{:doc "A graph representation of a type hierarchy usable for resolving the
            minimum representation for the product of two typed objects."
      :added "0.2.1"
      :author "Reid McKenzie"}
  me.arrdem.compiler.type-hierarchy
  (:require [clojure.set :as set]
            [loom.alg :as traversals]))

;;------------------------------------------------------------------------------
;; traversal & computation functions
(defn conversion-path
  "Resolves the type conversion sequence for going between two or more types.
   Returns a seq of sequences, being the intermediate types (if any) which each
   argument type must be cast through to reach the minimum common
   representation. Eg. a resolve-conversion for \"character\" and \"boolean\"
   should yield '((\"integer\") (\"integer\")) as the minimum common type is
   integer."
  [graph & types]
  (->> types
      (map (partial traversals/bf-traverse graph))
      (map set)
      (apply set/intersection)
      (map (apply juxt
                  (map #(partial traversals/shortest-path graph %1)
                       types)))
      (sort
       (comparator
        (fn [p1 p2] (<= (apply + (map count p1))
                        (apply + (map count p2))))))
      (first)))
