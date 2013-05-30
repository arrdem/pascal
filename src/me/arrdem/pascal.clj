(ns me.arrdem.pascal
  (:require [clojure.pprint :refer :all]
            (clojure.tools [logging :refer [info]]
                           [cli :refer [cli]])
            (me.arrdem.pascal [grammar :refer [pascal-program]]
                              [lexer :refer [pascal]]
                              [symtab :refer [pr-symtab with-symtab]])
            (me.arrdem.compiler [symtab]
                                [code-generation :as code])
            [name.choi.joshua.fnparse :as fnp])
  (:gen-class :main true))

(defn pr-code
  "A wrapper around pprint which makes its code-formatting setting more
   easily accessed."
  [c]
  (with-pprint-dispatch code-dispatch
    (pprint c)))

(defn pr-line
  "Prints an 80 char wide dash line."
  []
  (->> \-
       (repeat 78)
       (cons "; " )
       (apply str )
       println))

(defn build-ast
  "Wrapper around the grammar and the FNParse rule invocation to make throwing a
   token sequence at the grammar stack easy."
  [toks]
  (fnp/rule-match
   pascal-program
   #(println "FAILED: " %)
   #(println "LEFTOVER: " %2)
   {:remainder toks}))

(def process-string
  (comp build-ast pascal))

;;------------------------------------------------------------------------------

(defn parse-args [args]
  (cli args
       ["-genc"   "--[no-]gencode" :default true]
       ["-symtab" "--[no-]psymtab" :default false]))

(defn -main
  "The only valid arguments are targeted files. If there are no targeted files
   then decomp will target stdin as its token source."
  [& args]
  (let [[options args banner]
        (parse-args args)]
    (doseq [f (if-not (empty? args)
                args [*in*])]
      (with-symtab
        (info "attempting to read file" f)
        (-> f
            slurp
            process-string
            pr-code
            (#(if (:gencode options)
                (code/gencode %1)
                %1))
            )
        (if (:psymtab options)
          (do (pr-line)
              (pr-symtab)))
        nil))))
