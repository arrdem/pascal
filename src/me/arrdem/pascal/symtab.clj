(ns me.arrdem.pascal.symtab
  (:require [clojure.pprint :as pp]
            [me.arrdem.compiler.symtab :as cst]
            [me.arrdem.pascal.symtab.stdlib :as stdl]
            [me.arrdem.pascal.symtab.stdmacros :as stdm]
            [me.arrdem.pascal.symtab.stdtypes :as stdt]))

(def install!
  "Duplicate me.arrdem.compiler.symtab/install! into this namespace"
  me.arrdem.compiler.symtab/install!)

(def search
  "Duplicate me.arrdem.compiler.symtab/search into this namespace"
  me.arrdem.compiler.symtab/search)

(def ascend!
  "Duplicate me.arrdem.compiler.symtab/ascend! into this namespace"
  me.arrdem.compiler.symtab/ascend!)

(def descend!
  "Duplicate me.arrdem.compiler.symtab/descend! into this namespace"
  me.arrdem.compiler.symtab/descend!)

(def genlabel!
  "Duplicate me.arrdem.compiler.symtab/genlabel! into this namespace"
  me.arrdem.compiler.symtab/genlabel!)

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
     (reset! cst/*symns*  '())
     (init!)))

(def fmnt "%-10s : %s")

(defn pr-symtab
  "Pretty-prints the core symbol table. Indended for debugging, may be migrated
to compiler.symtab and linked here. Needs to be modified to print symbols in
some sort of namespace derived order."
  ([] (pr-symtab @cst/*symtab* '()))
  ([tbl stack]
     (for [[k v] tbl
           :when (string? k)
           :let  [indent 0
                  stack  (concat stack (list k))]]
       (let [vals (select-keys v [:value :type :type/data])]
         (if-not (empty? vals)
           (do (println (format fmnt (cst/render-ns stack)
                                (str vals)))
               (pr-symtab v stack)))))))
