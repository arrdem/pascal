(ns me.arrdem.pascal.parser.deliminators
  (:require [name.choi.joshua.fnparse     :as fnp]
            [me.arrdem.pascal.parser.util :as util]))

(util/deftoken comma :delim/comma)
(util/deftoken semicolon :delim/semi)
(util/deftoken colon :delim/colon)
(util/deftoken lparen :delim/lparen)
(util/deftoken rparen :delim/rparen)
(util/deftoken lbracket :delim/lbrack)
(util/deftoken rbracket :delim/rbrack)
(util/deftoken dotdot :delim/dotdot)
