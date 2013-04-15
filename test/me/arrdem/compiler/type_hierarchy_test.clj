(ns me.arrdem.compiler.type-hierarchy-test
  (:require [clojure.test :refer :all]
            [me.arrdem.pascal.types :refer :all]))

(deftest self-conversion
  (with-types (atom -type-graph)
    (doseq [t ["integer" "real" "character" "boolean"]]
      (is (= (conversion-path t t) [nil nil])))))

(deftest int-conversion
  (with-types (atom -type-graph)
    (is (= (conversion-path "character" "integer")
           ['("character" "integer") nil]))

    (is (= (conversion-path "boolean" "integer")
           ['("boolean" "integer") nil]))))
