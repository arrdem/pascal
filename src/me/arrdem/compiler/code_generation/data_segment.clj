(ns me.arrdem.compiler.code-generation.data-segment
  (:require [clojure.set :as s
                         :refer [union difference]]
            (me.arrdem.compiler.symbols [complex] [core] [records])
            [me.arrdem.compiler :refer [sizeof typeof valueof]]
            [me.arrdem.compiler.symtab :refer [gensym!]]))

;;------------------------------------------------------------------------------
;; preamble api
(defn preamble-ensure-installed
  "As the name suggests this function ensures that the argument symbol is
   installed in the code preamble to be generated by the argument state. Used
   to install floats, strings and even uninitialized memory with labeled
   addresses which can easily be manipulated by the rest of the code generation
   stack. Returns a pair (state, label) where label is the ID of the installed
   preable label."
  [state sym]
  (if (contains? (:installed (:preamble state)) sym)
    ;; already installed case, do nothing
    [state sym]
    ;; not installed case, generate an alignment directive and a dat directive.
    ;; then concat that to the :preamble :code value in state.
    (let [code [['comment (format "var %s" sym)]
                ['.align (sizeof (typeof sym))]
                ['.label sym]
                ['.space (sizeof sym)]]]
      [(-> state
           (update-in [:preamble :installed] union #{sym})
           (update-in [:preamble :code] concat code))
       sym])))

(defn preamble-install-float
  "Installs a floating point value into the preamble table, note that it also
   creates a :preamble :installed entry for that float. Makes no attempt to
   escap repetition of the same value."
  [state val]
  (let [sym (gensym!)
        code [['.label sym]
              ['.quad val]]]
  [(-> state
       (update-in [:preamble :code] concat code))
   sym]))

(defn preamble-install-string
  "Installs a string typed symbol into the preamble table."
  [state sym]
  [(-> state
       (update-in [:preamble :code] concat
                   [['.label sym]
                    ['.string (valueof sym)]]))
     sym])


(defn buid-preamble
  "Extracts the preamble code from a state map for prefixing or postfixing a
   code sequence. Intended to be the last operation on a state. Only returns the
   code seq."
  [state]
  (concat [;['comment "begin data segment"]
           ['.data]]
          (:code (:preamble state))))