(ns me.arrdem.pascal.symtab
  (:require [me.arrdem.compiler.symtab :as cst]
            [me.arrdem.compiler.namespace :as cns]
            [me.arrdem.pascal.symtab.stdlib :as stdl]
            [me.arrdem.pascal.symtab.stdmacros :as stdm]
            [me.arrdem.pascal.symtab.stdtypes :as stdt]))

(defn init! []
  (do (stdm/init!)
       (stdl/init!)
       (stdt/init!)))

(defmacro with-symtab
  [& forms]
  `(cst/with-symtab {}
     (cns/with-namespace '()
       (init!)
       ~@forms)))

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
