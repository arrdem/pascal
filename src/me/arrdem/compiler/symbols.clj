(ns me.arrdem.compiler.symbols
  (:require [me.arrdem.compiler :refer [nameof typeof sizeof addrof
                                        field-offset fields reftype
                                        follow valueof toString]]))

;; Primitives
;;    Primitives are dirt easy. They just resolve to themselves but they're
;;    typed and sized differently.
;;
;; Pointers
;;   Pointers are easy
;;   :name <name> - note that this will be "^<pointed type>"
;;   :type :reference
;;   :reference <type of pointed value>
;;   :size 4
;;
;; Records
;;   :name <name>
;;   :type :record
;;   :type/data <sequence of the types>
;;   :children <map of names to pairs [offset, type]>
;;
;; Arrays
;;   :name <name>
;;   :type :record
;;   :type/data <string being the number of indices concatenated with the type
;;               of the value at each index>
;;   :children <map of index to pairs [offset, type]>
;;
;;   Dealing with multi-dimensional arrays, we define new subtypes for the
;;   nested arrays, but note that we do not have to define symbol table recors
;;   for such sub-arrays. For an array such as a[1..5, 1..10, 1..15]:integer
;;
;;   type tree:
;;       {:name "integer" :size 8}
;;           ^- {:name "integer-15" :size (* 15 8)}
;;                  ^- {:name "integer-15-10" :size (* 10 15 8)}
;;                         ^- {:name "integer-15-10-5" :size (* 5 10 15 8)}
;;
;;   and the record:
;;       {:name "a" :type :record :type/data "integer-15-10-5" ...}

;;------------------------------------------------------------------------------
;; record types for type records
(defrecord PrimitiveType [name size-field]
  me.arrdem.compiler.ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self)))

(defrecord PointerType [name size-field ref]
  me.arrdem.compiler.ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self))
  me.arrdem.compiler.IPointer
    (reftype [self] (typeof (.ref self)))
    (follow [self] (.ref self)))

(defrecord ArrayType [name size-field children]
  me.arrdem.compiler.ISymbol
    (typeof [self] self)
    (nameof [self] (or (:qname self)
                       (.name self)))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self))
  me.arrdem.compiler.IIndexable
    (field-offset [self path]
      (->> path
           (map #(get (fields %1) %2)
                (.children self))
           (map addrof)
           (reduce + 0)))
    (fields [self] (.children self)))

;;------------------------------------------------------------------------------
;; Variable representation
(defrecord VariableType [name type val]
  me.arrdem.compiler.ISymbol
    (typeof [self] (.type self))
    (nameof [self] (or (:qname self)
                       (.name self)))
    (sizeof [self] (sizeof (typeof self)))
    (addrof [self] nil)
  me.arrdem.compiler.IIndexable
    (field-offset [self name]
      (field-offset (.type self) name))
    (fields [self]
      (fields (.type self)))
  me.arrdem.compiler.IPointer
    (reftype [self] (typeof (.type self)))
    (follow [self] (follow (.type self)))
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self))
  me.arrdem.compiler.IValued
    (valueof [self] (.val self)))

(defrecord RangeType [name range]
  me.arrdem.compiler.ISymbol
    (typeof [self] (nameof self))
    (nameof [self] (or (:qname self)
                       (.name self)))
    (sizeof [self] (sizeof (typeof self)))
    (addrof [self] nil)
  me.arrdem.compiler.IValued
    (valueof [self] (.range self)))

(defrecord RecordType [name members size-field]
  me.arrdem.compiler.ISymbol
    (typeof [self] self)
    (nameof [self] (or (:qname self)
                       (.name self)))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self))
  me.arrdem.compiler.IIndexable
    (field-offset [self name]
      (.offset (get (.members self) name)))
    (fields [self] (.members self)))

(defrecord RecordEntry [name type offset]
  me.arrdem.compiler.ISymbol
    (typeof [self] (.type self))
    (toString [self] (.name self))
    (nameof [self] (.name self))
    (sizeof [self] (sizeof (typeof self)))
    (addrof [self] (.offset self))
  me.arrdem.compiler.IIndexable
    (field-offset [self name]
      (field-offset (.type self) name))
    (fields [self] (fields (.type self)))
  me.arrdem.compiler.IPointer
    (reftype [self] (typeof (.type self)))
    (follow [self] (follow (.type self))))

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

(defrecord EnumType [name members val-type]
  me.arrdem.compiler.ISymbol
    (typeof [self] self)
    (nameof [self] (or (:qname self)
                       (.name self)))
    (sizeof [self] (sizeof (.val-type self)))
    (addrof [self] nil)
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self))
  me.arrdem.compiler.IValued
    (valueof [self] (keys (.members self)))
  me.arrdem.compiler.IIndexable
    (field-offset [self name]
      (.indexOf (apply list (keys (.members self))) name))
    (fields [self] (.members self)))

(defrecord ThinType [name type]
  me.arrdem.compiler.ISymbol
    (typeof [self] (typeof (.type self)))
    (nameof [self] (or (:qname self)
                       (.name self)))
    (sizeof [self] (sizeof (typeof self)))
    (addrof [self] nil)
  me.arrdem.compiler.IIndexable
    (field-offset [self name]
      (field-offset (.type self) name))
    (fields [self]
      (fields (.type self)))
  me.arrdem.compiler.IPointer
    (reftype [self] (typeof (.type self)))
    (follow [self] (follow (.type self)))
  me.arrdem.compiler.IValued
    (valueof [self] (valueof (.type self)))
  me.arrdem.compiler.IPPrinted
    (toString [self] (nameof self)))

;;------------------------------------------------------------------------------
;;Function representation

(defrecord FunctionType [name arity-and-type-set ret-type]
  me.arrdem.compiler.ISymbol
    (typeof [self] (.name self))
    (nameof [self] (.name self))
    (sizeof [self] nil)
    (addrof [self] nil)
  me.arrdem.compiler.IPPrinted
    (toString [self] (.name self))
  me.arrdem.compiler.IInvokable
    (arity [self] (map count (.arity-and-type-set self)))
    (valid-invokation? [self args]
      (if (contains? (.arity-and-type-set self) -1)
        true
        (contains? (.arity-and-type-set self)
                   args)))
    (return-type [self] (.ret-type self)))
