(ns me.arrdem.pascal.parser.util
  (:require [name.choi.joshua.fnparse :as fnp]))

(defmacro deftoken [symbol val]
  `(def ~symbol
     (fnp/term
      #(= (:lexington.tokens/type %1) ~val))))
