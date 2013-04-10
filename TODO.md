# TODO list
- Figure out _when_ to invoke macros, and make it so. Last thing done before the
  AST is yielded by the AST generation pipeline perhaps?

- Add tests for the macro system

- Note the convention that all AST expression heads are assumed to be macros, so
  even funcall which is really just a special case macro expanding into an
  argument pushing and popping function call.

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

- Rework the way that +, -, * and / generate in the IR, replacing them with
  -- M+, M-, M* and M/, being macros which do type checking and argument type conversion before expanding to
  -- float+ float- float* float/ int%
  -- int+ int- int* int/ int%
  -- This will be done at macro expansion time as part of type resolution. We
     aren't there yet, don't stress this bit but keep in mind.

- Add an "inlining" progn macro which attempts to eliminate nested progn groups
  by inserting their contents inline. Will probably just be a reduce over the
  body forms checking the first to see if it's 'progn and concat-ing rather
  than cons-ing.

- Add tests for the various grammar fragments

- Add type table and structure alignment tests

- Add a better pretty-printer now that it is possible to do a namespace based traversal of the symbol table.
- Add a better pretty-printer for code in general.

- Implement a way to dereference a pointer type
- Implement the nested dereference operation required for struct and array access to work as expected.

- Implement a debug library with selectable printing ala debug.h.

- Take grfrederik's advice and rework the entire symbol table system using the 
  old fasioned "pass a map around like nobody's buisness" approach as noted in 
  the git log at some point. This will be a major rework and may be difficult 
  due to fnparse but it should be done.
