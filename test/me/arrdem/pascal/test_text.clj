(ns me.arrdem.pascal.test-text)

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
   :symbols [{:name "i" :type/data "integer" :qname "graph1/i" :type :symbol}]})

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
                                  (progn (funcall "writeln" nil)
                                         (:= "graph1/i" (+ 1 "graph1/i"))
                                         (goto 0))))))
   :symbols [{:name "i" :type/data "integer" :qname "graph1/i" :type :symbol}
             {:name "lim" :type/data "integer" :qname "graph1/lim" :type :symbol}]})

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
                                                       (funcall "write" "graph1/str_0")
                                                       (:= "graph1/n" (- "graph1/n" 1))
                                                       (if (not (= "graph1/n" 0))
                                                         (goto 0)))
                                                (funcall "writeln" "graph1/str_1"))
                                         (:= "graph1/i" (+ 1 "graph1/i"))
                                         (goto 1))))))

   :symbols [{:name "d", :value 0.0625, :type :symbol, :type/data "real" :qname "graph1/d"}
             {:name "s", :value 32, :type :symbol, :type/data "integer" :qname "graph1/s"}
             {:name "h", :value 34, :type :symbol, :type/data "integer" :qname "graph1/h"}
             {:name "c", :value 6.28318, :type :symbol, :type/data "real" :qname "graph1/c"}
             {:name "lim", :value 32, :type :symbol, :type/data "integer" :qname "graph1/lim"}

             {:name "x", :type :symbol, :type/data "real" :qname "graph1/x"}
             {:name "y", :type :symbol, :type/data "real" :qname "graph1/y"}

             {:name "i", :type :symbol, :type/data "integer" :qname "graph1/i"}
             {:name "n", :type :symbol, :type/data "integer" :qname "graph1/n"}
             ]})
