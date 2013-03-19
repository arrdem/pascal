(ns me.arrdem.pascal
  (:require [clojure.pprint          :refer [pprint]]
            [me.arrdem.pascal.parser :refer [build-ast]]
            [me.arrdem.pascal.lexer  :refer [pascal]]
  )
  (:gen-class :main true))

(def process-string
  (comp build-ast pascal))

(defn -main
  "The only valid arguments are targeted files. If there are no targeted files
then decomp will target stdin as its token source."
  [& args]
  (if-not (empty? args)
    (doseq [f args]
      (-> f
          slurp
          process-string
          pprint)
      (println ""))

    (do (-> *in*
            java.io.BufferedReader.
            slurp
            process-string
            pprint)
        (println ""))))
