# TODO list
- Tweak the nameof protocol so that for all objects it will default to :qname
  over .name. this should make symbol table & code output more consistent.
- Test the symbol table make sure that vals are installing with qnames
- Make sure that the pprinter is doing its job, it seems to be dropping all
  type symbols probably due to the way that I handle map based records.
- Update the namespace pprinter to actually use pprint?
- Migrate me.arrdem.pascal.util into me.arrdem.pascal.lexer or .tokens?
- Clean up symbol_conversions.clj and sybols.clj into some more reasonable and
  modular structure?
- Rework me.arrdem.compiler.symtab to operate _either_ on a _rebindable_
  symbol or an argument state map ala congomongo.
- Bring the test suite up to date because it's waaay behind.
- Add absolute (static heap) allocation for VAR variables. Be able to compute 
  the heap address of a value via (addrof).

## back burner
- improve the output of the symbol table printer so as to be... reasonable
  -- make sure that for records it shows the offsets, sizes & fields

- move the "standard" type graph into the standard library symbol table code

- take another look at the lexington toolkit, see if there's a way to
  associate line & column metadata with tokens so that I can print information
  about the failure point rather than the failure and a bunch of garbage.

- Add tests for the macro system

- Add a way to determine whether a type conversion is required and perform it if so
  -- it seems to me that the easiest thing to do would be to recur from the left, taking
     the type of the leftmost sub-expression and asserting that any type T encountered is
     higher or equal to that base type converting where appropriate. As type conversion
     will have already taken place, then code generation is trivial because type is ensured.
     This should be a macro which is applied prior to code generation.
  -- Note that this requires a mechanism for determining the type of some
     arbitrary sub-expression recursively and then doing a type lattice lookup
     of the lowest common representation.
     -- That's dead easy, just add type metadata on all the Expression derived
        grammar productions. Throw a (with-type) wrapper into the types file and
        give it a similar (typeof). Do type resolution at expression building
        time, which shouldn't be hard as I've already finished all the type
        resolving code.

- Add tests for the various grammar fragments

- Add type table and structure alignment tests

- Add a better pretty-printer now that it is possible to do a namespace based traversal of the symbol table.
- Add a better pretty-printer for code in general.

- Implement a debug library with selectable printing ala debug.h.

- Take grfrederik's advice and rework the entire symbol table system using the
  old fasioned "pass a map around like nobody's buisness" approach as noted in
  the git log at some point. This will be a major rework and may be difficult
  due to fnparse but it should be done.
