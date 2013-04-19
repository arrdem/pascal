(ns me.arrdem.compiler.symbol-conversions
  (:require [me.arrdem.compiler :refer [sizeof nameof typeof fields
                                        field-offset reftype follow
                                        valueof]]
            [me.arrdem.compiler.symtab :refer [search]]))

;;------------------------------------------------------------------------------
;; Extensions for Clojure "primitives" of the compiler record protocols

(extend-protocol me.arrdem.compiler/ISymbol
  clojure.lang.PersistentArrayMap
    (typeof [self] (:type self))
    (nameof [self] (or (:qname self)
                       (:name self)))
    (sizeof [self] (or (:size self)
                       (:size (typeof self))))
    (addrof  [self] (:address self))

  clojure.lang.PersistentHashMap
    (typeof [self] (:type self))
    (nameof [self] (or (:name self)
                       (nameof (typeof self))))
    (sizeof [self] (:size (typeof self)))
    (addrof [self] (:address self))

  java.lang.String
    (typeof [self] (typeof (search self)))
    (nameof [self] self)
    (sizeof [self] (sizeof (search self)))
    (addrof [self] nil)

  Long
    (typeof [self] "integer")
    (nameof [self] "integer")
    (sizeof [self] 4)
    (addrof [self] nil)

  Integer
    (typeof [self] "integer")
    (nameof [self] "integer")
    (sizeof [self] 4)
    (addrof [self] nil)

  Double
    (typeof [self] "real")
    (nameof [self] "real")
    (sizeof [self] 8)
    (addrof [self] nil)

  Float
    (typeof [self] "real")
    (nameof [self] "real")
    (sizeof [self] 8)
    (addrof [self] nil))

(extend-protocol me.arrdem.compiler/IPointer
  String
    (reftype [self]
      (assert (= \^ (first self)))
      (apply str (rest self)))
    (follow [self] (search (reftype self))))

(extend-protocol me.arrdem.compiler/IIndexable
  String
    (field-offset [self field] (field-offset (search self) field))
    (fields [self] (fields (search self))))

(extend-protocol me.arrdem.compiler/IValued
  String
    (valueof [self] (valueof (search self))))
