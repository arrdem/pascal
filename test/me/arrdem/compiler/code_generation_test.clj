(ns me.arrdem.compiler.code-generation-test
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.code-generation :refer :all]))

(deftest use-reg-test
  (is (= (free-reg {:used-regs #{"a"}} "a")
         {:free-regs #{"a"}
          :used-regs #{}}))

  (is (= (free-reg {} "a")
         {})))
