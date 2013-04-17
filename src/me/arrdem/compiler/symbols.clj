(ns me.arrdem.compiler.symbols)

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

(defprotocol ISymbol
  (typeof [_] "Returns the type of the symbol")
  (nameof [_] "Returns the qualified name of the symbol")
  (sizeof [_] "Returns the size in bytes of the symbol")
  (addrof [_] "Returns an expression for the address of the symbol"))

(defprotocol IIndexable
  (field-offset [_ field] "Returns an expression for the address of the field")
  (fields [_] "Enumerates all fields as full ISymbols"))

(defprotocol IPointer
  (reftype [_] "Enumerates the type of the value to which it points")
  (follow [_] "Enumerates the targeted value as a full ISymbol"))

(defprotocol IValued
  (valueof [_] "Enumerates the value of the symbol, or an expression therefor"))

(defprotocol IPPrinted
  (toString [_] "Pretty printing for the TA's beinifit"))
;;------------------------------------------------------------------------------
;; record types for type records
(defrecord PrimitiveType [name size-field]
  ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.name self)))

(defrecord PointerType [name size-field reftype]
  ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.name self))
  IPointer
    (reftype [self] (.reftype self))
    (follow [_] nil))

(defrecord ArrayType [name size-field children]
  ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (.size-field self))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.name self))
  IIndexable
    (field-offset [self name]
      (.indexOf (apply list (keys (.children self))) name))
    (fields [self] (.children self)))

;;------------------------------------------------------------------------------
;; Variable representation
(defrecord VariableType [qname type val]
  ISymbol
    (typeof [self] (.type self))
    (nameof [self] (.qname self))
    (sizeof [self] (sizeof (typeof self)))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.qname self))
  IValued
    (valueof [self] (.val self)))

(defrecord RecordType [name members]
  ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (apply + (map sizeof (vals (.members self)))))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.name self))
  IIndexable
    (field-offset [self name]
      (.offset (get (.children self) name)))
    (fields [self] (.members self)))

(defrecord RecordEntry [name type offset]
  ISymbol
    (typeof [self] (.type self))
    (toString [self] (.name self))
    (nameof [self] (.name self))
    (sizeof [self] (.size-field (.type self)))
    (addrof [self] (.offset self))
  IIndexable
    (field-offset [self name]
      (field-offset (.type self) name))
    (fields [self] (fields (.type self))))

(defrecord EnumType [name members]
  ISymbol
    (typeof [self] self)
    (nameof [self] (.name self))
    (sizeof [self] (apply + (map sizeof (vals (.members self)))))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.name self))
  IIndexable
    (field-offset [self name]
      (.indexOf (apply list (keys (.children self))) name))
    (fields [self] (.members self)))

(defrecord ThinType [qname type]
  ISymbol
    (typeof [self] (typeof (.type self)))
    (nameof [self] (.qname self))
    (sizeof [self] (sizeof (typeof self)))
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.qname self)))
;;------------------------------------------------------------------------------
;;Function representation
(defprotocol IInvokable
  (arity [self] "Returns the arity of the callable record")
  (valid-invokation? [self arg-type-list]
    "Tests an argument type sequence for arity and type")
  (return-type [self] "Returns the return type of the callable record"))

(defrecord FunctionType [name arity-and-type-set ret-type]
  ISymbol
    (typeof [self] (.name self))
    (nameof [self] (.name self))
    (sizeof [self] nil)
    (addrof [self] nil)
  IPPrinted
    (toString [self] (.name self))
  IInvokable
    (arity [self] (map count (.arity-and-type-set self)))
    (valid-invokation? [self args]
      (contains? (.arity-and-type-set self)
                 (map typeof args)))
    (return-type [self] (.ret-type self)))

;;------------------------------------------------------------------------------
;; Extensions for Clojure "primitives"
(extend String
  ISymbol
    {:typeof (fn [self] (str "char-" (count self)))
     :nameof (fn [self] self)
     :sizeof (fn [self] (count self))
     :addrof (fn [self] nil)})

(extend Long
  ISymbol
    {:typeof (fn [self] "integer")
     :nameof (fn [self] "integer")
     :sizeof (fn [self] 4)
     :addrof (fn [self] nil)})

(extend Integer
  ISymbol
    {:typeof (fn [self] "integer")
     :nameof (fn [self] "integer")
     :sizeof (fn [self] 4)
     :addrof (fn [self] nil)})

(extend Double
  ISymbol
    {:typeof (fn [self] "real")
     :nameof (fn [self] "real")
     :sizeof (fn [self] 8)
     :addrof (fn [self] nil)})

(extend Float
  ISymbol
    {:typeof (fn [self] "real")
     :nameof (fn [self] "real")
     :sizeof (fn [self] 8)
     :addrof (fn [self] nil)})

(extend clojure.lang.PersistentArrayMap
  ISymbol
    {:typeof :type
     :nameof (fn [self] (or (:qname self)
                            (:name self)))
     :sizeof (fn [self] (or (:size self)
                            (:size (typeof self))))
     :addrof :address})

(extend clojure.lang.PersistentHashMap
  ISymbol
    {:typeof :type
     :nameof (fn [self] (or (:name self)
                            (nameof (typeof self))))
     :sizeof (fn [self] (:size (typeof self)))
     :addrof :address})
