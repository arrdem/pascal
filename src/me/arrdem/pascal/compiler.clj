(ns ^{:doc "A wrapper around me.arrdem.compiler which creates structures for
            managing the compiler structure within fnparse via side-effects
            due to the difficulty of properly passing around a state record
            within the fnparse rules jumble."
      :added "0.3.0"
      :author "Reid McKenzie"}
      me.arrdem.pascal.compiler
  (:require [clojure.pprint :as pp]
            [me.arrdem.macros :refer [-<> -<n>]]
            [me.arrdem.compiler :as compiler]
            [me.arrdem.compiler.symtab :as cst]
            [me.arrdem.pascal.symtab.stdlib :as stdl]
            [me.arrdem.pascal.symtab.stdmacros :as stdm]
            [me.arrdem.pascal.symtab.stdtypes :as stdt]))

;;------------------------------------------------------------------------------
;; Symbol table mutable state crap
(def ^:dynamic ^:private state  nil)

(defn init!
  "Installs the basic Pascal symbols and type conversions to the compiler state.
   May or may not handle types and macros as well, may or may not pass those off
   to type and macro specific initializers elsewhere."
  ([]
     (init! {}))
  ([m]
     (def ^:dynamic ^:private state
       (-> m
           (assoc :gensym 0)
           (assoc :label 0)
           (stdm/init!)
           (stdl/init!)
           (stdt/init!)))))

(def clear!
  "Re-initializes the entire compiler state."
  init!)

;;------------------------------------------------------------------------------
;; me.arrdem.compiler wrappers
(defn install!
  "Wraps me.arrdem.compiler.symtab/install!, concealing the side-effect on
   *compiler* from user code to present the same API as before."
  [record]
  (-<> record
      (cst/install! state <>)
      (do (def ^:dynamic ^:private state <>)
          <>)))

(defn search
  "Duplicate me.arrdem.compiler.symtab/search into this namespace"
  [name-or-record]
  (cst/search name-or-record))

(defn ascend!
  "Duplicate me.arrdem.compiler/ascend! into this namespace"
  [top-ns]
  (def ^:dynamic ^:private state
    (compiler/ascend! state top-ns)))

(defn descend!
  "Duplicate me.arrdem.compiler/descend! into this namespace"
  [top-ns]
  (def ^:dynamic ^:private state
    (compiler/descend! state)))

(defn genlabel!
  "Duplicate me.arrdem.compiler/genlabel! into this namespace"
  [top-ns]
  (def ^:dynamic ^:private state
    (compiler/genlabel! state)))

(defn gensym!
  "Duplicate me.arrdem.compiler/gensym! into this namespace"
  [top-ns]
  (def ^:dynamic ^:private state
    (compiler/gensym! state)))

(def fmnt "%%-%ss : %%s")

(defn pr-symtab
  "Pretty-prints the core symbol table. Indended for debugging, may be migrated
to compiler.symtab and linked here. Needs to be modified to print symbols in
some sort of namespace derived order."
  ([] (pr-symtab (compiler/get-symtab state)  '()))
  ([tbl stack]
     (doseq [[k v] tbl]
       (when (string? k)
         (let [indent (* 7 (inc (count stack)))
               fmnt (format fmnt indent)
               stack  (concat stack (list k))
               vals (select-keys v [:value :type :type/data])]
           (if-not (empty? vals)
             (println (format fmnt (cst/render-ns stack)
                              (pr-str vals))))
           (when (map? v)
             (pr-symtab v stack)))))))
