(ns me.arrdem.pascal.symtab)

(def base_st
  {
;;------------------------------------------------------------------------------
;; Predefined types
   '("real")     {:name "real"     :type :basic :size 8}
   '("integer")  {:name "integer"  :type :basic :size 4}
   '("char")     {:name "char"     :type :basic :size 1}
   '("boolean")  {:name "boolean"  :type :basic :size 4}

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
;; Predefined macros
;; ("functions") which need to be expanded via some sort of macro system in
;; order to properly express their runtime behavior: ex. new() which requires
;; type data.
;;
;; MACROS:
;;   Macros shall have the type :macro, and the key :fn. As with lisp macros,
;;   macros will be invoked with arguments equal to the tail of the base
;;   form and are expected to return either an atom, or a sequence which will be
;;   assumed contain macros and will be macroexpanded until such time as the
;;   returned value is no longer a list, or is empty, or the expand is equal to
;;   the input. Also, macros shall only be defined at the top level and for this
;;   pascal compiler are not user-definable. Hardcoded only.

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
   :label 0
   :gensym 0
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
  (atom (list "toplevel")))

(def ^:dynamic *symtab*
  "Used to track all symbols."
  (atom (-> base_st
            (assoc :label 0)
            (assoc :gensym 0))))

(defn genlabel!
  "Generates and returns an integer label, side-effecting the :label count of the
*symtab* registry."
  ([] (:label
       (swap! *symtab*
              update-in [:label] inc))))

(defn render-ns
  "Renders the *symns* stack to a prefix string for symbols."
  ([] (render-ns @*symns*))
  ([stack] (str (apply str (interpose \. (reverse (next stack))))
                "/" (first stack))))

(defn install!
  "Installs a symbol map (created by the caller) in the *symtab* registry
providing name qualification appropriate to the *symns* stack."
  [sym]
  (let [path (conj @*symns* (:name sym))]
    (println path)
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
  "Recursively searches the symbol table for a symbol with an argument name.
Returns the symbol map if such a symbol exists. Failure behavior is undefined,
but the returning a nil value and throwing an exception are both acceptable."
  ([sym] (search sym @*symns*))
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

(defn pmacroexpand
  "A quick and dirty implementation of an \"outermost first\" macroexpand. Looks
up macros from the symbol table, and applies them if possible."
  [expr]
  (if (seq? expr)
    (let [expander (pmacroexpand (first expr))
          expander (if (symbol? expander)
                     (name expander)
                     (str expander))
          expander (:fn (search expander))
          res      (if expander
                     (apply expander (rest partial))
                     (cons (first expr) (next partial)))
          res      (cons (first res) (map pmacroexpand (rest res)))]
      (if expander
        (pmacroexpand res)
        res))
    expr))
