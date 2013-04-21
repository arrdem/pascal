(ns me.arrdem.pascal.symtab
  (:require [clojure.pprint :as pp]
            [me.arrdem.compiler.symtab :as cst]
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

(def fmnt-0 "")
(def fmnt-1 "")

;; shitty-pprint is designed to produce output like this:
;; :a
;;     :b -> 3
;;     :c -> 4
;;     :d
;;         :a -> 3
;;         :b -> 4
;;         :c -> 'foo

(defn make-prefix [prefix str]
  (if (= 0 (count prefix))
    str
    (concat prefix "/" str)))

(defn shitty-pprint
  [prefix map]
  (let [indent (-> prefix count (repeat " ") ((partial apply str)))
        {members true fields false} (group-by (comp map? second) map)]

    (doseq [[k v] fields]
      (println ";" indent k "=>" v))

    (doseq [[k v] members]
      (let [my-prefix (->> k
                           str
                           (make-prefix prefix)
                           (apply str))]
        (println "; " my-prefix)
        (shitty-pprint my-prefix (-> v
                                     (assoc :class (type v))))
        (println ";")))))

(defn pr-symtab []
  (shitty-pprint "" @cst/*symtab*))
