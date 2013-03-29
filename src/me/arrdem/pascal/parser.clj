(ns me.arrdem.pascal.parser
  (:require [clojure.pprint :as pp]
            [me.arrdem.pascal.lexer :refer [pascal]]
            [me.arrdem.pascal.grammar :refer [pascal-program]]
            [name.choi.joshua.fnparse :as fnp]
            ))

(defn build-ast [toks]
  (fnp/rule-match
   pascal-program
   #(println "FAILED: " %)
   #(println "LEFTOVER: " %2)
   {:remainder toks}))
