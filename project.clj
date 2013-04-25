(defproject me.arrdem/pascal "0.3.2"
  :description "A Pascal lexer, parser and AST generator"
  :url "http://github.com/arrdem/pascal"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [factual/fnparse "2.3.0"]
                 [jkkramer/loom "0.2.0"]
                 [lexington "0.1.1"]]
  :main me.arrdem.pascal)
