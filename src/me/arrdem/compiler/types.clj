(ns me.arrdem.compiler.types
  (:require [me.arrdem.compiler :refer [sizeof typeof nameof]]
            [me.arrdem.compiler.symbols :refer [->RecordEntry
                                                map->RecordType]]))

;;------------------------------------------------------------------------------
;; Code for computing field alignments

(defn aligned-offset [t o]
  (let [s (sizeof t)
        off-mod (int (/ o s))]
    (* s
       (+ (if (= (* off-mod s) o) 0 1)
          off-mod))))

(defn align-struct [members]
  (reduce
   (fn [state-map entry]
     (let [o (aligned-offset (typeof entry)
                             (:size-field state-map))
           n (->RecordEntry (nameof entry) (typeof entry) o)]
       (-> state-map
           (assoc :size-field (+ o (sizeof (typeof entry))))
           (assoc-in [:members (nameof entry)] n))))
   {:size-field 0} members))

(defn ->RecordType [name members]
  (-> members
      align-struct
      (assoc :name name)
      map->RecordType))
