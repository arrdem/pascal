(ns ^{:doc    "A more structured setting for the various functions which are
               used to manipulate and generate abstract syntax tree elements.
               Intended for use as a more elegant backend to semantics, and
               as a utility suite for macros when they come down the pipe."
      :author "Reid McKenzie"
      :added  "0.2.0"}
  me.arrdem.pascal.ast)

(defn ecomp [fx fn]
  `(~fn ~fx))

(defn e-> [v & fns]
  (reduce ecomp v fns))
