(ns me.arrdem.pascal.test-util)

(defmacro symtab-wrapper [& forms]
  `(binding [me.arrdem.pascal.symtab/*symns* (atom '("toplevel"))
             me.arrdem.pascal.symtab/*symtab* (atom me.arrdem.pascal.symtab/base_st)]
     ~@forms))
