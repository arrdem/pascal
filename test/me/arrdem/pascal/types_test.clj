(ns me.arrdem.pascal.types-test
  (:require [clojure.test :refer :all]

            [me.arrdem.pascal.types :refer :all]))

(deftest self-conversion
  (with-types (atom -type-graph)
    (doseq [t ["integer" "real" "character" "boolean"]]
      (is (= (conversion-path t t) '(() ()) )))))

(deftest int-conversion
  (with-types (atom -type-graph)
    (is (= (first (conversion-path "character" "integer"))
           '(("integer") ())
           ))

    (is (= (first (conversion-path "boolean" "integer"))
           '(("integer") ())
           ))
    ))
