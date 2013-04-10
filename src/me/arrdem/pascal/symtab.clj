(ns ^{:doc "A wrapper around me.arrdem.compiler which creates structures for
            managing the compiler structure within fnparse via side-effects
            due to the difficulty of properly passing around a state record
            within the fnparse rules jumble."
      :added "0.1.0"
      :author "Reid McKenzie"}
      me.arrdem.pascal.symtab
  (:require [clojure.pprint :as pp]
            [me.arrdem.compiler :as compiler]
            [me.arrdem.compiler.symtab :as cst]
            [me.arrdem.pascal.symtab.stdlib :as stdl]
            [me.arrdem.pascal.symtab.stdmacros :as stdm]
            [me.arrdem.pascal.symtab.stdtypes :as stdt]))

(def install!
  "Duplicate me.arrdem.compiler.symtab/install! into this namespace"
  cst/install!)

(def search
  "Duplicate me.arrdem.compiler.symtab/search into this namespace"
  cst/search)

(def ascend!
  "Duplicate me.arrdem.compiler/ascend! into this namespace"
  compiler/ascend!)

(def descend!
  "Duplicate me.arrdem.compiler/descend! into this namespace"
  compiler/descend!)

(def genlabel!
  "Duplicate me.arrdem.compiler/genlabel! into this namespace"
  compiler/genlabel!)

(def gensym!
  "Duplicate me.arrdem.compiler/gensym! into this namespace"
  compiler/gensym!)

(defn init!
  "Installs the basic Pascal symbols and type conversions to the symbol table.
May or may not handle types and macros as well, may or may not pass those off to
type and macro specific initializers elsewhere."
  ([]
     (stdm/init!)
     (stdl/init!)
     (stdt/init!)))

(defmacro with-p-symtab
  [& forms]
  `(binding [cst/*symtab* (atom {})
             cst/*symns* (atom (list))]
     (init!)
     ~@forms))

(defn clear!
  "Nukes the symbol table, replacing it with the Pascal basic table as defined
above. Not sure why you would need this as the typical case is single program
invocation per compile batch but here it is anyway."
  ([]
     (reset! cst/*symtab* {})
     (reset! cst/*symns* '())
     (init!)))

(def fmnt "%%-%ss : %%s")

(defn pr-symtab
  "Pretty-prints the core symbol table. Indended for debugging, may be migrated
to compiler.symtab and linked here. Needs to be modified to print symbols in
some sort of namespace derived order."
  ([] (pr-symtab @cst/*symtab* '()))
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
