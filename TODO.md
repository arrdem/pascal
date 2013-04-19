# TODO list
## todo & buggy
- improve the output of the symbol table printer so as to be... reasonable
  -- make sure that for records it shows the offsets, sizes & fields
- add type conversion via macros over arithmetic expressions
- add type metadata to all arithmetic expressions
- move the "standard" type graph into the standard library symbol table code
- improve the type conversion code to transform into an arbitrary
  representation rather than strict supersets as the current type transform is
  designed to do what with being directed and all.
- track down && manually inspect the current failure point, it seems to be
  something to do with my type system resolving to a nil value incorrectly on
  line 41 of "pasrec.pas". Not sure where this is coming from but looks to be
  something screwy in the way that the type of an array entry is resolved. Fix
  it so I can finish and submit.
- take another look at the lexington toolkit, see if there's a way to
  associate line & column metadata with tokens so that I can print information
  about the failure point rather than the failure and a bunch of garbage.


## back burner
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
