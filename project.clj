(defproject me.arrdem/pascal (slurp "VERSION")
  :description "A Pascal lexer, parser and AST generator"
  :url "http://github.com/arrdem/pascal"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure   "1.4.0"]
                 [factual/fnparse       "2.3.0"]
                 [lexington             "0.1.1"]
                 [jkkramer/loom         "0.2.0"]]
  :main me.arrdem.pascal)
