(ns me.arrdem.pascal.parser.operators
  (:require [name.choi.joshua.fnparse     :as fnp]
            [me.arrdem.pascal.parser.util :as util]))

(util/deftoken add :op/add)
(util/deftoken sub :op/sub)
(util/deftoken mul :op/mul)
(util/deftoken div :op/div)
(util/deftoken assign :op/assign)
(util/deftoken eq :op/eq)
(util/deftoken neq :op/ne)
(util/deftoken lt :op/lt)
(util/deftoken le :op/le)
(util/deftoken ge :op/ge)
(util/deftoken gt :op/gt)
(util/deftoken point :op/point)
(util/deftoken dot :op/dot)
