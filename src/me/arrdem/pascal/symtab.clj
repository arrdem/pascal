(ns me.arrdem.pascal.symtab
  (:require [me.arrdem.compiler.symtab]))

(def pascal_base_st
  {
;;------------------------------------------------------------------------------
;; Predefined functions
   '("exp")      {:name "exp"      :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("tfexp")    {:name "trexp"    :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("sin")      {:name "sin"      :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("cos")      {:name "cos"      :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("trsin")    {:name "trsin"    :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("sqrt")     {:name "sqrt"     :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("round")    {:name "round"    :type :fn    :type/ret "real"    :type/arg ["real"]}
   '("iround")   {:name "iround"   :type :fn    :type/ret "integer" :type/arg ["real"]}
   '("ord")      {:name "ord"      :type :fn    :type/ret "integer" :type/arg ["integer"]}
   '("new")      {:name "new"      :type :fn    :type/ret "integer" :type/arg ["integer"]}
   '("trnew")    {:name "trnew"    :type :fn    :type/ret "integer" :type/arg ["integer"]}
   '("write")    {:name "write"    :type :fn    :type/ret nil       :type/arg ["char"]}
   '("writeln")  {:name "writeln"  :type :fn    :type/ret nil       :type/arg ["charsym"]}
   '("writef")   {:name "writef"   :type :fn    :type/ret nil       :type/arg ["real"]}
   '("writelnf") {:name "writelnf" :type :fn    :type/ret nil       :type/arg ["real"]}
   '("writei")   {:name "writei"   :type :fn    :type/ret nil       :type/arg ["integer"]}
   '("writelni") {:name "writelni" :type :fn    :type/ret nil       :type/arg ["integer"]}
   '("read")     {:name "read"     :type :fn    :type/ret nil       :type/arg []}
   '("readln")   {:name "readln"   :type :fn    :type/ret nil       :type/arg []}
   '("eof")      {:name "eof"      :type :fn    :type/ret "boolean" :type/arg []}

;;------------------------------------------------------------------------------
;; Type conversion functions
   '("ctoi")     {:name "ctoi"     :type :fn    :type/ret "integer" :type/arg ["char"]}
   '("btoi")     {:name "btoi"     :type :fn    :type/ret "integer" :type/arg ["boolean"]}
   '("itof")     {:name "itof"     :type :fn    :type/ret "real"    :type/arg ["integer"]}

;;------------------------------------------------------------------------------
;; Variables
;; There are (for obvious reasons) no pre-defined variables, but this is a spec
;; for what a variable entry must contain.
;;
;;   {:name       <string  name of the symbol>
;;    :type       :symbol ; this is non-negotiable
;;    :type/data  <type of the value stored here,
;;                 being a basic type or a pointer thereto>
;;    :type/value <initial value of the symbol or nil if none>
;;   }
  })

(defmacro with-p-symtab
  [& forms]
  `(binding [me.arrdem.compiler.symtab/*symtab* (atom pascal_base_st)]
     ~@forms))

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

(defn clear!
  "Nukes the symbol table, replacing it with the Pascal basic table as defined
above. Not sure why you would need this as the typical case is single program
invocation per compile batch but here it is anyway."
  ([]
     (reset! me.arrdem.compiler.symtab/*symtab* pascal_base_st)))

(defn pr-symtab
  "Pretty-prints the core symbol table. Indended for debugging, may be migrated
to compiler.symtab and linked here. Needs to be modified to print symbols in
some sort of namespace derived order."
  ([]
     (doseq [[k v] @me.arrdem.compiler.symtab/*symtab*]
       (println (format fmnt (str k) (str v))))))
