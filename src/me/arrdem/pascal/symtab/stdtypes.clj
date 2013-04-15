(ns me.arrdem.pascal.symtab.stdtypes
  (:require [me.arrdem.compiler.symtab :refer [search install!]]))

;; Primitives
;;    Primitives are dirt easy. They just resolve to themselves but they're
;;    typed and sized differently.
;;
;; Pointers
;;   Pointers are easy
;;   :name <name> - note that this will be "^<pointed type>"
;;   :type :reference
;;   :reference <type of pointed value>
;;   :size 4
;;
;; Records
;;   :name <name>
;;   :type :record
;;   :type/data <sequence of the types>
;;   :children <map of names to pairs [offset, type]>
;;
;; Arrays
;;   :name <name>
;;   :type :record
;;   :type/data <string being the number of indices concatenated with the type
;;               of the value at each index>
;;   :children <map of index to pairs [offset, type]>
;;
;;   Dealing with multi-dimensional arrays, we define new subtypes for the
;;   nested arrays, but note that we do not have to define symbol table recors
;;   for such sub-arrays. For an array such as a[1..5, 1..10, 1..15]:integer
;;
;;   type tree:
;;       {:name "integer" :size 8}
;;           ^- {:name "integer-15" :size (* 15 8)}
;;                  ^- {:name "integer-15-10" :size (* 10 15 8)}
;;                         ^- {:name "integer-15-10-5" :size (* 5 10 15 8)}
;;
;;   and the record:
;;       {:name "a" :type :record :type/data "integer-15-10-5" ...}

(defn init!
  "Function of no arguments which serves simply to populate the symbol table
with the standard 'primitive' types which Pascal supports."
  ([]
     (doseq [t [
                ;;--------------------------------------------------------------
                ;; Basic types
                {:name "integer" :type :primitive :size 4}
                {:name "char" :type :primitive :size 1}
                {:name "boolean" :type :primitive :size 4}
                {:name "real":type :primitive :size 8}

                ;;--------------------------------------------------------------
                ;; Pointer types
                {:name "^integer"
                 :type :reference
                 :reference "integer"
                 :size 4}

                {:name "^char"
                 :type :reference
                 :reference "char"
                 :size 4}

                {:name "^boolean"
                 :type :reference
                 :reference "boolean"
                 :size 4}

                {:name "^real"
                 :type :reference
                 :reference "real"
                 :size 4}
                ]]
       (install! t))))
