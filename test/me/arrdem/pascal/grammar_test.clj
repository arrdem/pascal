(ns me.arrdem.pascal.grammar-test
  (:require [clojure.test :refer :all]
            [me.arrdem.compiler.symtab :refer [search install!]]
            [me.arrdem.compiler.symbols :refer [nameof typeof fields]]
            [me.arrdem.pascal :refer [process-string build-ast]]
            [me.arrdem.pascal.symtab :refer [init! clear!]]
            [me.arrdem.pascal.lexer :refer [pascal]]
            [name.choi.joshua.fnparse :as fnp]))

(deftest pointer-def-case
  (binding [me.arrdem.compiler.symtab/*symtab* (atom {})]
    (clear!)
    (install! {:name "^foo" :type "integer"})
    (let [res (-> (fnp/rule-match
                   me.arrdem.pascal.grammar/variable-declaration
                   #(println "FAILED: " %)
                   #(println "LEFTOVER: " %2)
                   {:remainder (pascal "var i, j, k : ^ integer;
                                       l, m, n : ^ foo")})
                  rest rest)]
      (doseq [i res]
        (let [r (search i)]
          (is (= i
                 (nameof r))
              "Is the installed really installed?"))))))

(deftest simple-record-def-case
  (binding [me.arrdem.compiler.symtab/*symtab* (atom {})]
    (clear!)
    (let [int (search "integer")
          res (name.choi.joshua.fnparse/rule-match
                   me.arrdem.pascal.grammar/variable-declaration
                   prn prn
                   {:remainder (me.arrdem.pascal.lexer/pascal
                                "var i, j, k : integer;
                                     l : record h, y : real end")})]
      (doseq [i ["i" "j" "k"]]
        (let [r (search i)]
          (is (= i
                 (nameof r))
              "Normal var did not install correctly")
          (is (= int
                 (typeof r))
              "Normal var did not register its type correctly")))
      (let [r (search "l")]
        (is (= true
               (instance? me.arrdem.compiler.symbols.VariableType r))
            "Did the record install at all?")
        (is (= "record__0"
               (nameof (typeof r)))
            "Did the record install as a gensym named record?")
        (is (= #{"y" "h"}
               (set (keys (fields (typeof r)))))
            "Did the record get the correct fields?")))))

(deftest enum-def-case
  (binding [me.arrdem.compiler.symtab/*symtab* (atom {})]
    (clear!)
    (let [res (fnp/rule-match
               me.arrdem.pascal.grammar/ptype
               #(println "FAILED: " %)
               #(println "LEFTOVER: " %2)
               {:remainder (pascal "(red, white, blue)")})]
      (is (= true
             (instance? me.arrdem.compiler.symbols.EnumType res))
          "Is the result an enum?")
      (doseq [m (keys (fields res))]
        (let [m (search m)]
          (is (= true
                 (instance? me.arrdem.compiler.symbols.VariableType m))
              "Is the enum value installed as its own symbol?")
          (is (= true
                 (instance? me.arrdem.compiler.symbols.EnumType (typeof m)))
              "Is the installed value clearly part of the enum structure?"))))))
