(ns me.arrdem.compiler)

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
  (toString [_] "Pretty printing for the TA's benefit"))
