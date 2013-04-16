(ns me.arrdem.pascal.lexer
    (:require [lexington.lexer :refer :all]
              [lexington.utils.lexer :refer :all]
              [me.arrdem.pascal.tokens :refer [pascal-base]]
              [me.arrdem.pascal.util :refer [strfn readerfn]]))

;; TODO:
;; Add some way to count the line and character number for ever token
;; this way errors could be localized to a line & word. If I had
;; decent errors that is...

(def pascal
  (-> pascal-base
      (discard :ws)
      (discard :nl)
      (discard :pcomment)
      (discard :bcomment)

      (generate-for :identifier :val (comp (partial apply str)
                                           :lexington.tokens/data))
      (generate-for :pstring    :val strfn)
      (generate-for :intnum     :val readerfn)
      (generate-for :floatnum   :val readerfn)

      (generate-for :op_add     :val (constantly '+))
      (generate-for :op_sub     :val (constantly '-))
      (generate-for :op_or      :val (constantly 'or))
      (generate-for :op_mul     :val (constantly '*))
      (generate-for :op_mod     :val (constantly 'mod))
      (generate-for :op_div     :val (constantly '/))
      (generate-for :op_and     :val (constantly 'and))
      (generate-for :op_assign  :val (constantly ':=))
      (generate-for :op_eq      :val (constantly '=))
      (generate-for :op_ne      :val (constantly '<>))
      (generate-for :op_lt      :val (constantly '<))
      (generate-for :op_le      :val (constantly '<=))
      (generate-for :op_ge      :val (constantly '>=))
      (generate-for :op_gt      :val (constantly '>))
      (generate-for :op_point   :val (constantly  (symbol "^")))
      (generate-for :op_dot     :val (constantly '.))
      ))
