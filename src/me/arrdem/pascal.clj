(ns me.arrdem.pascal
  (:require [clojure.pprint :refer :all]
            [me.arrdem.pascal.grammar :refer [pascal-program]]
            [me.arrdem.pascal.lexer :refer [pascal]]
            [me.arrdem.pascal.symtab :refer [pr-symtab init!]]
            [me.arrdem.compiler.symtab]
            [name.choi.joshua.fnparse :as fnp])
  (:gen-class :main true))

(defn pr-code
  "A wrapper around pprint which makes its code-formatting setting more
easily accessed."
  ([c]
     (with-pprint-dispatch code-dispatch
       (pprint c))))

(defn pr-line
  "Prints an 80 char wide dash line."
  ([]
     (println (apply str (repeat 80 \-)))))

(defn build-ast
  "Wrapper around the grammar and the FNParse rule invocation to make throwing a
token sequence at the grammar stack easy."
  ([toks]
     (fnp/rule-match
      pascal-program
      #(println "FAILED: " %)
      #(println "LEFTOVER: " %2)
      {:remainder toks})))

(def process-string
  (comp build-ast pascal))

(defn -main
  "The only valid arguments are targeted files. If there are no targeted files
then decomp will target stdin as its token source."
  ([& args]
     (init!)
     (if-not (empty? args)
       (doseq [f args]
         (println "attempting to read file" f)
         (-> f
             slurp
             process-string
             pr-code)
         (pr-line)
         (pr-symtab)
         nil)

       (do (-> *in*
               slurp
               process-string
               pr-code)
           (pr-line)
           (pr-symtab)
           nil))))
