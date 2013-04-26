(ns ^{:doc "Defines some relatively complex ISymbol structures which are used to
            provide enums, ranges, varaibles and arrays within the symbol table.
            Also provides ThinType which is essential to type aliasing."
      :author "Reid McKenzie"
      :added "0.3.3"}
  me.arrdem.compiler.symbols.complex
  (:require [me.arrdem.compiler :refer [nameof typeof sizeof addrof
                                        field-offset fields reftype
                                        follow valueof toString]]))

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
