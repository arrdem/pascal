(ns me.arrdem.compiler.code-generation.registers-tests
  (:require [clojure.test :refer [deftest is]]
            [me.arrdem.compiler.code-generation.registers :refer :all]))

(deftest free-reg-test
  (is (= (free-reg {:used-regs #{"a"}} "a")
         {:free-regs #{"a"}
          :used-regs #{}}))

  (is (= (free-reg {} "a")
         {})))

(deftest use-reg-test
  (is (= (use-reg {:used-regs #{}
                   :free-regs #{"a"}}
                  "a")
         {:used-regs #{"a"}
          :free-regs #{}})))

(deftest register?-test
  (is (= (register? {:used-regs #{}
                     :free-regs #{"a"}}
                    "a")
         true))
  (is (= (register? {:used-regs #{"a"}
                     :free-regs #{}}
                    "a")
         true))
  (is (= (register? {:used-regs #{}
                     :free-regs #{}}
                    "a")
         false)))
