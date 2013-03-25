(ns me.arrdem.pascal.semantics
  (:require [me.arrdem.pascal.symtab :refer [genlabel! search]]
            [name.choi.joshua.fnparse :as fnp]))

(defn binop [args]
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

(defn makeupfor [[_0 id _1 Vi _2 Vf _3 stmnts]]
  (let [update `(~':= ~(makederef id) (~'+ ~(makederef id) 1))
        lstart (genlabel!)
        lend   (genlabel!)]
    `((~'label ~lstart)
      (~'if (~'>= ~(makederef id) ~Vf)
            (~'goto ~lend))
      (~'do
        ~'stmnts)
      ~update
      (~'goto ~lstart)
      (~'label ~lend))))

(defn makedownfor [[_0 id _1 Vi _2 Vf _3 stmnts]]
  (let [update `(~':= ~(makederef id) (~'- ~(makederef id) 1))
        lstart (genlabel!)
        lend   (genlabel!)]
    `((~'label ~lstart)
      (~'if (~'<= ~(makederef id) ~Vf)
            (~'goto ~lend))
      (~'do
        ~'stmnts)
      ~update
      (~'goto ~lstart)
      (~'label ~lend))))
