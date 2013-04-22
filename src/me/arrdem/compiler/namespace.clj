(ns me.arrdem.compiler.namespace)

;;------------------------------------------------------------------------------
;; The namespace stack

(def ^:dynamic *symns*
  ""
  (atom '()))

(defn descend
  "Pushes the argument namespace onto the *symns* stack, altering how symbols
   are resolved until the *symns* stack is poped. Used for recuring into
   function and program definitions which may have local bindings."
  [stack ns]
  (concat stack (list ns)))

(defn ascend
  ""
  [stack]
  (butlast stack))

(defn reset-symns!
  "Nukes the *symns* value restoring it to its base state. Usefull for testing,
   multiple compile runs without restart."
  []
  (reset! *symns* (list)))

(defn render-ns
  "Renders a ns stack to a prefix string for symbols."
  [stack]
  (if (< 1 (count stack))
    (str (apply str (interpose \. (butlast stack)))
         "/" (last stack))
    (first stack)))

(defn decomp-ns
  "Unrenders a namespace"
  [name]
  (split name #"[\./]"))
