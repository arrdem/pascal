(ns ^{:doc "Defines various primitives for interacting with type records in the
            symbol table, as well as for initializing them and computing
            properties such as alignment."
      :added "0.1.5"
      :author "Reid McKenzie"}
      me.arrdem.compiler.types)

;;------------------------------------------------------------------------------
;; Code for computing field alignments
(defn aligned-offset [t o]
  (let [s       (:size t)
        off-mod (int (/ o s))]
    (* s
       (+ (if (= (* off-mod s) o) 0 1)
          off-mod))))

(defn align-struct [members]
  (loop [Ms   members
         o    0
         offs []]
    (let [f  (first Ms)
          To (aligned-offset f o)]
      (if (not (empty? (rest Ms)))
        (recur (rest Ms)
               (+ To (:size f))
               (cons To offs))
        (map #(assoc %1 :offset %2)
             members
             (cons To offs))))))

;;------------------------------------------------------------------------------
;; Type constructors
(defn BasicType [name size]
  (-> {:type :basic :members {}}
      (assoc :name name)
      (assoc :size size)))

(defn RecordType [name member-type-pairs]
  (let [pairs      (map (fn [[x y]] (assoc y :name x)) member-type-pairs)
        alignments (align-struct pairs)
        size       (let [m (first (sort #(compare
                                          (:offset %2)
                                          (:offset %1))
                                        alignments))]
                     (+ (:offset m) (:size m)))]
  (-> {:type :record
       :name name}
      (assoc :members (reduce (fn [m i]
                                (assoc m (:name i) i))
                              {} alignments))
      (assoc :size size))))

;;------------------------------------------------------------------------------
;; API for interacting with types and type records
(def sizeof   :size)
(def typeof   :type)
(def nameof   :name)
(def children :members)
