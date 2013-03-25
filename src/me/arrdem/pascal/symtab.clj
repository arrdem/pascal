(ns me.arrdem.pascal.symtab)

(def ^:private base_st
  {"real"     {:name "real"     :type :basic :size 8}
   "integer"  {:name "integer"  :type :basic :size 4}
   "char"     {:name "char"     :type :basic :size 1}
   "boolean"  {:name "boolean"  :type :basic :size 4}
   "exp"      {:name "exp"      :type :fn    :type/ret "real"    :type/arg ["real"]}
   "tfexp"    {:name "trexp"    :type :fn    :type/ret "real"    :type/arg ["real"]}
   "sin"      {:name "sin"      :type :fn    :type/ret "real"    :type/arg ["real"]}
   "cos"      {:name "cos"      :type :fn    :type/ret "real"    :type/arg ["real"]}
   "trsin"    {:name "trsin"    :type :fn    :type/ret "real"    :type/arg ["real"]}
   "sqrt"     {:name "sqrt"     :type :fn    :type/ret "real"    :type/arg ["real"]}
   "round"    {:name "round"    :type :fn    :type/ret "real"    :type/arg ["real"]}
   "iround"   {:name "iround"   :type :fn    :type/ret "integer" :type/arg ["real"]}
   "ord"      {:name "ord"      :type :fn    :type/ret "integer" :type/arg ["integer"]}
   "new"      {:name "new"      :type :fn    :type/ret "integer" :type/arg ["integer"]}
   "trnew"    {:name "trnew"    :type :fn    :type/ret "integer" :type/arg ["integer"]}
   "write"    {:name "write"    :type :fn    :type/ret nil       :type/arg ["char"]}
   "writeln"  {:name "writeln"  :type :fn    :type/ret nil       :type/arg ["charsym"]}
   "writef"   {:name "writef"   :type :fn    :type/ret nil       :type/arg ["real"]}
   "writelnf" {:name "writelnf" :type :fn    :type/ret nil       :type/arg ["real"]}
   "writei"   {:name "writei"   :type :fn    :type/ret nil       :type/arg ["integer"]}
   "writelni" {:name "writelni" :type :fn    :type/ret nil       :type/arg ["integer"]}
   "read"     {:name "read"     :type :fn    :type/ret nil       :type/arg []}
   "readln"   {:name "readln"   :type :fn    :type/ret nil       :type/arg []}
   "eof"      {:name "eof"      :type :fn    :type/ret "boolean" :type/arg []}})

(def ^:rebindable ^:dynamic *ns*
  "Used to track the namespace levels above the current point of evaluation.
An empty list signifies that we are operating at the \"top\" level where program
forms and other such values live. It is here that the \"standard library\" lives.
When decending to another namespace the decend! function is called and a value is
pushed onto the head of this list. When returning from a nested namespace, the
rise! function is called which pops the top element off of this stack.

Symbol resolution is performed by iteratively prefixing the symbol to be resolved
with the concatonation of the stack, searching and poping until either the symbol
is resolved, or the stack is empty."
  (atom (list)))

(def ^:rebindable ^:dynamic *symtab*
  "Used to track all symbols."
  (atom base_st))

(declare genlabel! install!)
