(ns me.arrdem.pascal.symtab
  (:require [me.arrdem.compiler.symtab :refer :all]))

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
