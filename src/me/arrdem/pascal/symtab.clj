(ns me.arrdem.pascal.symtab
  (:require [me.arrdem.compiler.symtab :as cst]
            [me.arrdem.pascal.symtab.stdlib :as stdl]
            [me.arrdem.pascal.symtab.stdmacros :as stdm]
            [me.arrdem.pascal.symtab.stdtypes :as stdt]))

(defn init!
  "Installs the basic Pascal symbols and type conversions to the symbol table.
May or may not handle types and macros as well, may or may not pass those off to
type and macro specific initializers elsewhere."
  ([]
     (stdm/init!)
     (stdl/init!)
     (stdt/init!)))

(defmacro with-p-symtab
  [& forms]
  `(binding [cst/*symtab* (atom {})
             cst/*symns* (atom (list))]
     (init!)
     ~@forms))

(defn clear!
  "Nukes the symbol table, replacing it with the Pascal basic table as defined
above. Not sure why you would need this as the typical case is single program
invocation per compile batch but here it is anyway."
  ([]
     (reset! cst/*symtab* {})
     (reset! cst/*symns* '())
     (init!)))

(defn make-prefix [prefix str]
  (if (= 0 (count prefix))
    str
    (concat prefix "/" str)))


;; shitty-pprint is designed to produce output like this:
;; ; :a
;; ;    :b => 3
;; ;    :c => 4
;; ;    :d
;; ;        :a => 3
;; ;        :b => 4
;; ;        :c => 'foo
;;
;; TODO:
;; - the ";" is hard-coded not an unbound prefix argument
;; - no attempt is made to align by 4spc or 8spc indentation
;; - the "=>" is hard coded
;; - translate this to run atop https://github.com/brandonbloom/fipp, it should
;;   be pretty straightforward and would generally be awesome
;;
;; But it does handle the non-recursive case first, print the class of the data
;; and provide the ";" prefix, which is more than I could say about the previous
;; pretty printer I was using.

(defn shitty-pprint
  [prefix map]
  (let [indent (-> prefix count (repeat " ") ((partial apply str)))
        {members true fields false} (group-by (comp map? second) map)]

    (doseq [[k v] fields]
      (println ";" indent k "=>" v))

    (doseq [[k v] members]
      (let [my-prefix (->> k str (make-prefix prefix) (apply str))]
        (println "; " my-prefix)
        (shitty-pprint my-prefix (-> v
                                     (assoc :class (type v))))
        (println ";")))))

(defn pr-symtab []
  (shitty-pprint "" @cst/*symtab*))
