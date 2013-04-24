(ns me.arrdem.pascal.test-text
  (:require [me.arrdem.compiler.symbols]))

(def pint {:name "integer" :size 4})
(def preal {:name "real" :size 8})

;;------------------------------------------------------------------------------
;; working test cases (supposedly)

(def triv-pas
  {:text (slurp "test/me/arrdem/pascal/cases/triv.pas")
   :ast (read-string (slurp "test/me/arrdem/pascal/cases/triv.tree"))
   :symbols [{:type pint :qname "graph1/i"}]})

(def trivb-pas
  {:text (slurp "test/me/arrdem/pascal/cases/trivb.pas")
   :ast (read-string (slurp "test/me/arrdem/pascal/cases/trivb.tree"))
   :symbols [{:type pint :qname "graph1/i"}
             {:type pint :qname "graph1/lim"}]})

(def graph1-pas
  {:text (slurp "test/me/arrdem/pascal/cases/graph1.pas")
   :ast (read-string (slurp "test/me/arrdem/pascal/cases/graph1.tree"))
   :symbols [{:value 0.0625, :type preal :qname "graph1/d"}
             {:value 32, :type pint :qname "graph1/s"}
             {:value 34, :type pint :qname "graph1/h"}
             {:value 6.28318, :type preal :qname "graph1/c"}
             {:value 32, :type pint :qname "graph1/lim"}

             {:type preal :qname "graph1/x"}
             {:type preal :qname "graph1/y"}

             {:type pint :qname "graph1/i"}
             {:type pint :qname "graph1/n"}
             ]})

(def pasrec-pas
  {:text (slurp "test/me/arrdem/pascal/cases/pasrec.pas")
   :ast (read-string (slurp "test/me/arrdem/pascal/cases/pasrec.tree"))
   :symbols []})
