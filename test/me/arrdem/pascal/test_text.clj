(ns me.arrdem.pascal.test-text
  (:require [me.arrdem.compiler.symbols]))

(def pint {:name "integer" :size 4})
(def preal {:name "real" :size 8})

;;------------------------------------------------------------------------------
;; working test cases (supposedly)

(def triv-pas
  {:text "{ trivial test program        -- file triv.pas }
          program graph1(output);
            var i:integer;
          begin
            i := 3
          end."
   :ast '(program "graph1"
                  (progn "output")
                  (comment "defined variables" "graph1/i")
                  (progn (:= "graph1/i" 3)))
   :symbols [{:type pint :qname "graph1/i"}]})

(def trivb-pas
  {:text "program graph1(output);
            var i,lim : integer;
          begin
            lim := 7;
            for i := 0 to lim do
              writeln('*')
          end."
   :ast '(program "graph1" (progn "output")
                  (comment "defined variables" "graph1/i" "graph1/lim")
                  (progn (:= "graph1/lim" 7)
                         (progn (label 0)
                                (:= "graph1/i" 0)
                                (if (<= "graph1/i" "graph1/lim")
                                  (progn (funcall "writeln" "graph1/str_0")
                                         (:= "graph1/i" (+ 1 "graph1/i"))
                                         (goto 0))))))
   :symbols [{:type pint :qname "graph1/i"}
             {:type pint :qname "graph1/lim"}]})

;;------------------------------------------------------------------------------
;; future test cases

(def graph1-pas
  {:text "{ program 4.9 from Jensen & Wirth       -- file pastst.pas }
         program graph1(output);
           const d = 0.0625; {1/16, 16 lines for interval [x,x+1]}
                 s = 32; {32 character widths for interval [y,y+1]}
                 h = 34; {character position of x-axis}
                 c = 6.28318; {2*pi}  lim = 32;
           var x,y : real;  i,n : integer;
         begin
           for i := 0 to lim do
           begin
             x := d*i;
             y := exp(-x)*sin(c*x);
             n := round(s*y) + h;
             repeat
               write(' ');
               n := n-1
             until n = 0;
             writeln('*')
           end
         end."
   :ast '(program "graph1"
                  (progn "output")
                  (comment "got constant decl group:"
                           "graph1/d"
                           "graph1/s"
                           "graph1/h"
                           "graph1/c"
                           "graph1/lim")
                  (comment  "defined variables"
                            "graph1/x" "graph1/y"
                            "graph1/i"
                            "graph1/n")
                  (progn (progn (label 1)
                                (:= "graph1/i" 0)
                                (if (<= "graph1/i" "graph1/lim")
                                  (progn (progn (:= "graph1/x" (* "graph1/d" "graph1/i"))
                                                (:= "graph1/y" (* (funcall "exp" (* -1 "graph1/x"))
                                                                  (funcall "sin" (* "graph1/c" "graph1/x"))))
                                                (:= "graph1/n" (+ (funcall "round" (* "graph1/s" "graph1/y")) "graph1/h"))
                                                (progn (label 0)
                                                       (progn (funcall "write" "graph1/str_0")
                                                              (:= "graph1/n" (- "graph1/n" 1)))
                                                       (if (not (= "graph1/n" 0))
                                                         (goto 0)))
                                                (funcall "writeln" "graph1/str_1"))
                                         (:= "graph1/i" (+ 1 "graph1/i"))
                                         (goto 1))))))

   :symbols [{:value 0.0625, :type preal :qname "graph1/d"}
             {:value 32, :type pint :qname "graph1/s"}
             {:value 34, :type pint :qname "graph1/h"}
             {:value 6.28318, :type preal :qname "graph1/c"}
             {:value 32, :type pint :qname "graph1/lim"}

             {:type preal :qname "graph1/x"}
             {:type preal :qname "graph1/y"}

             {:type pint :qname "graph1/i"}
             {:type pint :qname "graph1/n"}
             ]})
