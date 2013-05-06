(ns me.arrdem.compiler.codegen-tests
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.codegen :refer :all]))

(def testcode
  '(progn (:= a (+ 1 5))
          (:= b (* a 10))
          (:= c (* b a))))

(def teststate
  {:registers {}
   :memory {'a 0x00 'b 0x04 'c 0x08}
   :free-registers x86-64-regs
   })
