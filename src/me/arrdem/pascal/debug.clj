(ns me.arrdem.pascal.debug)

;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------

(def ^:dynamic *rule-stack* (atom '("toplevel")))
(def ^:dynamic indent-width  (atom 0))

(defn stack []
  (let [s (apply str (interpose "-> " (reverse @*rule-stack*)))]
    (reset! indent-width (count s))
    s))

(defn debug [& msg]
  (let [prefix (stack)
        indent (apply str (repeat @indent-width " "))]
    (println prefix)
    (doseq [m msg]
      (println indent m))))
