(ns me.arrdem.compiler.types)

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
;; Basic types

(def preal    (BasicType "real"    8))
(def pinteger (BasicType "integer" 4))
(def pbool    (BasicType "boolean" 4))
(def pchar    (BasicType "char"    1))

(def ^:dynamic *types*
  (atom {"real" preal
         "integer" pinteger
         "boolean" pbool
         "char" pchar}))

;;------------------------------------------------------------------------------
;; API for interacting with types and type records

(def sizeof   :size)
(def typeof   :type)
(def nameof   :name)
(def children :members)

(defn install!
  "Installs a type in the types table, tracing the types of the members to
compute the total size of the record, as well as the indexing offsets to its
members. Does not check to see if the type name is already taken."
  ([t]
     (swap! *types* (:name t) t)))

(defn search
  "Searches the type table for a given type. Note that type names are
unqualified and types are considered to be global unlike variables."
  ([name]
     (get @*types* name)))
