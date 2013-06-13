(ns me.arrdem.compiler.type-hierarchy-test
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.type-hierarchy :refer :all]
            [me.arrdem.pascal.types :refer [-type-graph]]
            [loom.graph :as graph]))

(deftest self-conversion
  (doseq [t ["integer" "real" "alpha" "boolean"]]
    (is (= (conversion-path -type-graph t t) [nil nil]))))

(deftest int-conversion
  (is (= (conversion-path -type-graph "alpha" "integer")
         ['("alpha" "integer") nil]))

  (is (= (conversion-path -type-graph "boolean" "integer")
         ['("boolean" "integer") nil])))
