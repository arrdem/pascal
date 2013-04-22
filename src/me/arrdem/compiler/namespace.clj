(ns me.arrdem.compiler.namespace
  (:require [clojure.string :refer [split]]))

(def ^:dynamic *namespace*
  (atom '()))

(defn descend
  "Pushes the argument namespace onto the *symns* stack, altering how symbols
   are resolved until the *symns* stack is poped. Used for recuring into
   function and program definitions which may have local bindings."
  [stack ns]
  (concat stack (list ns)))

(defn ascend
  [stack]
  (butlast stack))

(defn render-ns
  "Renders a ns stack to a prefix string for symbols."
  [stack]
  (if (< 1 (count stack))
    (str (apply str (interpose \. (butlast stack)))
         "/" (last stack))
    (first stack)))

(defn decomp-ns
  "Splits a namespace string into a namespace path"
  [name]
  (split name #"[\./]"))


;;------------------------------------------------------------------------------
;;------------------------------------------------------------------------------
;; side-effectful wrapers around the namespace operations
(defmacro with-namespace
  [stack & body]
  `(binding [*namespace* (atom '())]
    ~@body))

(defn set-namespace!
  [stack]
  (alter-var-root #'*namespace*
                  (constantly (atom stack))))

(defn namespace!
  [& fns]
  (with-namespace
    (set-namespace! '())
    (doseq [f fns]
      (f))))

(defn ascend! []
  (swap! *namespace* ascend))

(defn descend! [sub-ns]
  (swap! *namespace* descend sub-ns))

(defn get-ns []
  @*namespace*)

(defn reset-symns!
  "Nukes the *symns* value restoring it to its base state. Usefull for testing,
   multiple compile runs without restart."
  []
  (reset! *namespace* (list)))
