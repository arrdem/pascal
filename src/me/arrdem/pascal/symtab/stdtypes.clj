(ns me.arrdem.pascal.symtab.stdtypes
  (:require [me.arrdem.compiler.symtab :refer [search install!]]))

;; Primitives
;;    Primitives are dirt easy. They just resolve to themselves but they're
;;    typed and sized differently.
;;
;; Pointers
;;   Pointers are easy, just type em as :reference rather than :record or
;;   :primitive and then use the :type/data field to record the type of
;;   the dereference.
;;
;; Arrays
;;   Arrays are a bitch... maily because I have to support arbitrary indexing.
;;   Blergh. I suppose I could treat . references as special cases of some
;;   general reference operation which allows for multivariate types...
;;   Oh. Easy mode. I just need to def a set of nested index types when I see
;;   an array ref and then treat a[i, j, k] as if it's a.i.j.k where i, j and k
;;   may be integer values... yeah that'll do it.

(defn init!
  "Function of no arguments which serves simply to populate the symbol table
with the standard 'primitive' types which Pascal supports."
  ([]
     (doseq [t [
                ;;--------------------------------------------------------------
                ;; Basic types
                {:name "integer"  :type :primitive :type/data "integer" :size 4}
                {:name "char"     :type :primitive :type/data "integer" :size 1}
                {:name "boolean"  :type :primitive :type/data "integer" :size 4}
                {:name "real"     :type :primitive :type/data "real"    :size 8}

                ;;--------------------------------------------------------------
                ;; Pointer types
                {:name "integer^" :type :reference :type/data "integer" :size 4}
                {:name "char^"    :type :reference :type/data "char"    :size 4}
                {:name "boolean^" :type :reference :type/data "boolean" :size 4}
                {:name "real^"    :type :reference :type/data "real"    :size 4}
                ]]
       (install! t))))
