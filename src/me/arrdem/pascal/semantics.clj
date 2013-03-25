(ns me.arrdem.pascal.semantics
  (:require [me.arrdem.pascal.symtab :refer [genlabel! search]]
            [name.choi.joshua.fnparse :as fnp]))

(defn binop [args]
  (println args)
  (let [[e0 op e1] args]
    `(~op ~e0 ~e1)))

(defn makeif [[_0 test _1 s elsepart]]
  (let [else (if elsepart (second elsepart))]
    `(~'if ~test ~s ~else)))

(defn makewhile [[_0 test _1 s]]
  (let [label (genlabel!)]
    `((~'label ~label)
      (~'if ~test
          (~'do ~s
              (~'goto ~label))))))

(defn makerepeat [[_0 s _1 test]]
  (let [label (genlabel!)]
    `((~'label ~label)
      ~s
      (~'if (~'not ~test)
            (~'goto ~label)))))

(defn makederef [sym]
  `(~'deref ~(symbol (:qname (search sym)))))
