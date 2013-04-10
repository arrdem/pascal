(ns ^{:doc "Defines various primitives for interacting with type records in the
            symbol table, as well as for initializing them and computing
            properties such as alignment."
      :added "0.1.5"
      :author "Reid McKenzie"}
  me.arrdem.compiler.types)

;;------------------------------------------------------------------------------
;; API for interacting with types and type records
(def sizeof :size)
(def typeof :type)
(def nameof :name)
(def children :members)

;;------------------------------------------------------------------------------
;; Code for computing field alignments
(defn aligned-offset [t o]
  (let [off-mod (int (/ o (sizeof t)))]
    (* (sizeof t)
       (+ (if (= (* off-mod (sizeof t)) o) 0 1)
          off-mod))))

(defn align-struct [members]
  (loop [Ms members
         o 0
         offs []]
    (let [f (first Ms)
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
  {:type :basic
   :members {}
   :name name
   :size size})

(defn RecordType [name member-type-pairs]
  (let [alignments (-> member-type-pairs
                       (map (fn [[x y]] (assoc y :name x)))
                       align-struct)]
    {:type :record
     :name name
     :members (reduce (fn [m i] (assoc m (:name i) i))
                      {} alignments)
     :size (-> alignments
               ((partial sort #(apply compare (map :offset %))))
               first
               (#(apply + ((juxt :offset :size) %1))))}))
