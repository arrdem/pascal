(ns me.arrdem.compiler.types
  (:require [me.arrdem.compiler.symbols :refer [sizeof typeof nameof
                                                ->RecordEntry]]))

;;------------------------------------------------------------------------------
;; Code for computing field alignments

(defn aligned-offset [t o]
  (let [s (sizeof t)
        off-mod (int (/ o s))]
    (* s
       (+ (if (= (* off-mod s) o) 0 1)
          off-mod))))

(defn align-struct [members]
  (->> members
      (reduce
       (fn [state-map entry]
         (let [o (aligned-offset entry (:offset state-map))
               n (->RecordEntry (nameof entry) (typeof entry) o)]
           (-> state-map
               (assoc :offset (+ o (sizeof entry)))
               (assoc (nameof entry) n))))
       {:offset 0})
      ((fn [x] (dissoc x :offset)))))

(defn old-align-struct [members]
  (loop [Ms   members
         o    0
         offs []]
    (let [f  (first Ms)
          To (aligned-offset f o)]
      (if (not (empty? (rest Ms)))
        (recur (rest Ms)
               (+ To (sizeof f))
               (cons To offs))
        (map #(assoc %1 :offset %2)
             members
             (cons To offs))))))
