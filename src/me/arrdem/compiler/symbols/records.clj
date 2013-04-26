(ns ^{:doc "Contains the code used to create and align struct records prior to
            their installation in the symbol table. Isolated due to the
            overriding of ->RecordType from the default."
      :author "Reid McKenzie"
      :added "0.3.3"}
      me.arrdem.compiler.symbols.records
  (:require [me.arrdem.compiler :refer :all]))

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
