(ns me.arrdem.compiler.symbol-conversions
  (:require [me.arrdem.compiler :refer [sizeof nameof typeof fields
                                        field-offset reftype follow
                                        valueof]]
            [me.arrdem.compiler.symtab :refer [search]]))

;;------------------------------------------------------------------------------
;; provide most interfaces over strings for ease of use..

(extend-type java.lang.String
  me.arrdem.compiler/ISymbol
    (typeof [self] (typeof (search self)))
    (nameof [self] self)
    (sizeof [self] (sizeof (search self)))
    (addrof [self] nil)

  me.arrdem.compiler/IPointer
    (reftype [self]
      (let [refname (nameof (typeof self))]
        (assert (= \^ (first refname)))
        (apply str (rest refname))))
    (follow [self] (search (reftype self)))

  me.arrdem.compiler/IIndexable
    (field-offset [self field] (field-offset (search self) field))
    (fields [self] (fields (search self)))

  me.arrdem.compiler/IValued
    (valueof [self] (valueof (search self))))

;;------------------------------------------------------------------------------
;; Extensions for Clojure "primitives" of the compiler record protocols

(extend-protocol me.arrdem.compiler/ISymbol
  clojure.lang.PersistentArrayMap
    (typeof [self] (:type self))
    (nameof [self] (or (:qname self)
                       (:name self)
                       (nameof (typeof self))))
    (sizeof [self] (or (:size self)
                       (sizeof (typeof self))))
    (addrof  [self] (:address self))

  clojure.lang.PersistentHashMap
    (typeof [self] (:type self))
    (nameof [self] (or (:qname self)
                       (:name self)
                       (nameof (typeof self))))
    (sizeof [self] (or (:size self)
                       (sizeof (typeof self))))
    (addrof [self] (:address self))

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
    (addrof [self] nil)

  Object
    (typeof [self] (typeof (meta self)))
    (nameof [self] (nameof (meta self)))
    (sizeof [self] (sizeof (meta self)))
    (addrof [self] nil))
