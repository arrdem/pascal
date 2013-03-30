(ns me.arrdem.pascal.lexer-test
  (:require [clojure.test :refer :all]

            [name.choi.joshua.fnparse :refer [rule-match]]

            [me.arrdem.pascal.lexer :refer :all]
            [me.arrdem.pascal.tokens :refer :all]))

(deftest identifiers
  (let [words ["integer" "real" "float"
               "string" "foobar" "frotz"]
        lexed (map pascal words)
        fnparsed (map (fn [x] (rule-match identifier
                                          identity identity
                                          {:remainder x}))
                      lexed)
        results (map vector words fnparsed)]
    (doseq [[i o] results]
      (is (= i o)
          (str i " did not parse to an identifier correctly!")))))
