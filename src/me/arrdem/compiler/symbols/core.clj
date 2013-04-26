(ns ^{:doc "a containder for the basic symbol table types. Contains the
            PrimitiveType used to represent machine literals, the FunctionType,
            and the PointerType."
      :author "Reid McKenzie"
      :added "0.3.3"}
  me.arrdem.compiler.symbols.core
  (:require [me.arrdem.compiler :refer :all]))

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
