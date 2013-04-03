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
   :symbols [{:name "i" :type/data "integer" :qname "graph1/i"}]})

(def trivb-pas
  {:text "program graph1(output);
            var i,lim : integer;
          begin
            lim := 7;
            for i := 0 to lim do
              writeln('*')
          end."
   :ast '(program "graph1"
                  (progn "output")
                  (comment "defined variables"
                           "graph1/i"
                           "graph1/lim")
                  (progn (:= "graph1/lim" 7)
                         (progn (label 1)
                                (:= "graph1/i" 0)
                                (if (<= "graph1/lim" "graph1/i")
                                  (goto 2))
                                (funcall "writeln" "graph1/str_1")
                                (:= "graph1/i" (+ 1 "graph1/i"))
                                (goto 1)
                                (label 2))))
   :symbols [{:name "i" :type/data "integer" :qname "graph1/i"}
             {:name "lim" :type/data "integer" :qname "graph1/lim"}]})

;;------------------------------------------------------------------------------
;; future test cases

(def graph1.pas
  {:text
   "{ program 4.9 from Jensen & Wirth       -- file pastst.pas }
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
   :ast []

   :symbols []})
