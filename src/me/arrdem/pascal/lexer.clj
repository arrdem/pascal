(ns me.arrdem.pascal.lexer
    (:require [lexington.lexer       :refer :all]
              [lexington.utils.lexer :refer :all]
              [me.arrdem.pascal.tokens :refer [pascal-base]]
              [me.arrdem.pascal.util   :refer [strfn readerfn]]))

(def pascal
  (-> pascal-base
      (discard :ws)
      (discard :pcomment)
      (discard :bcomment)

      (generate-for :identifier :val readerfn)
      (generate-for :pstring    :val strfn)
      (generate-for :intnum     :val readerfn)
      (generate-for :floatnum   :val readerfn)
      ))
