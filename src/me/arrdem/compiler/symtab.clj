(ns me.arrdem.compiler.symtab
  (:require [clojure.string :refer [split]]))

(def base_st
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

;;------------------------------------------------------------------------------
;; Values
;; These are "magic" values which various parts of the compiler rely on.

   :label  0 ;; counter used for label generation
   :gensym 0 ;; counter shared by all symbol generation

   })

(def ^:dynamic *symns*
  "Used to track the namespace levels above the current point of evaluation.
An empty list signifies that we are operating at the \"top\" level where program
forms and other such values live. It is here that the \"standard library\" lives.
When decending to another namespace the decend! function is called and a value is
pushed onto the head of this list. When returning from a nested namespace, the
rise! function is called which pops the top element off of this stack.

Symbol resolution is performed by iteratively prefixing the symbol to be resolved
with the concatonation of the stack, searching and poping until either the symbol
is resolved, or the stack is empty."
  (atom '()))

(defn descend!
  "Pushes the argument namespace onto the *symns* stack, altering how symbols are
resolved until the *symns* stack is poped. Used for recuring into function
and program definitions which may have local bindings."
  [ns] (swap! *symns* conj ns))

(defn ascend!
  "Pops the *synms* stack, taking any symbols defined in a nested ns out of
scope. Invoked when returning from function and program definitions as they may
contain symbol bindings."
  [] (swap! *symns* pop))

(def ^:dynamic *symtab*
  "Used to track all symbols."
  (atom base_st))

(defn genlabel!
  "Generates and returns an integer label, side-effecting the :label count of the
*symtab* registry."
  ([] (:label
       (swap! *symtab*
              update-in [:label] inc))))

(defn render-ns
  "Renders the *symns* stack to a prefix string for symbols."
  ([] (render-ns @*symns*))
  ([stack]
     (if (< 1 (count stack))
       (str (apply str (interpose \. (reverse (next stack))))
            "/" (first stack))
       (first stack))))

(defn install!
  "Installs a symbol map (created by the caller) in the *symtab* registry
providing name qualification appropriate to the *symns* stack."
  [sym]
  (let [path (conj @*symns* (:name sym))]
    (println ";    installed to path" path "\n")
    (swap! *symtab* assoc path sym)))

(defn gensym!
  "Generates a symbol name (string) which is guranteed by use of an incrementing
counter to be unique to the current compile session. Optionally takes a string
prefix for the generated name which does not effect the numeric part of the
name. Returns a string being the prefix argument or \"G__\" followed by the
string render of the gensym counter before it was incremented."
  ([] (gensym! "G__"))
  ([s] (str s
            (:gensym
             (swap! *symtab*
                    update-in [:gensym] inc)))))

(defn search
  "A wrapper around stack-search wich provides the base case logic required to
parse fully qualified names into a full stack path. Defaults to using
stack-search before returning a failure result."
  [name]
  (let [stack (reverse (split name #"[\./]"))]
    (get @*symtab* stack
         (stack-search name))))

(defn stack-search
  "Recursively searches the symbol table for a symbol with an argument name.
Returns the symbol map if such a symbol exists. Failure behavior is undefined,
but the returning a nil value and throwing an exception are both acceptable."
  ([sym] (stack-search sym @*symns*))
  ([sym stack]
     (let [qualified-sym (conj stack sym)
           rstack        (if-not (empty? stack) (pop stack))]
       (or (if-let [v (get @*symtab* qualified-sym)]
             (assoc v :qname (render-ns qualified-sym))
             (if (empty? stack)
               (do (println "WARNING: SYMBOL" qualified-sym "NOT FOUND")
                 nil)))
           (if-not (empty? stack)
             (recur sym rstack))))))
