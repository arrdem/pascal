(ns me.arrdem.pascal.debug)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(def ^:dynamic rule-stack    (atom '("toplevel")))
(def ^:dynamic indent-width  (atom 0))
(def ^:dynamic prn-prefix    (atom true))

(defn stack []
  (let [s (apply str (interpose "-> " (reverse @rule-stack)))]
    (reset! indent-width (count s))
    s))

(defn scope-pop []
  (swap! rule-stack clojure.core/pop)
  (reset! prn-prefix true))

(defn push [s]
  (swap! rule-stack conj s)
  (reset! prn-prefix true))

(defn debug [& msg]
  (let [prefix (stack)
        indent (apply str (repeat @indent-width " "))]
    (if @prn-prefix
      (println prefix))
    (reset! prn-prefix false)
    (doseq [m msg]
      (println indent m))))
