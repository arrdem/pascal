# TODO list
- Add tests for the macro system
- Add a type lattice using (https://github.com/jkk/loom)
- Add a way to determine whether a type conversion is required and perform it if so
  -- it seems to me that the easiest thing to do would be to recur from the left, taking
     the type of the leftmost sub-expression and asserting that any type T encountered is
     higher or equal to that base type converting where appropriate. As type conversion
     will have already taken place, then code generation is trivial because type is ensured.
     This should be a macro which is applied prior to code generation.
  -- Note that this requires a mechanism for determining the type of some 
     arbitrary sub-expression recursively and then doing a type lattice lookup
     of the lowest common representation.
- Rework the way that +, -, * and / generate in the IR, replacing them with
  -- M+, M-, M* and M/, being macros which do type checking and argument type conversion before expanding to
  -- float+ float- float* float/ int%
  -- int+ int- int* int/ int%
- Add an "inlining" progn macro which attempts to eliminate nested progn groups
  by inserting their contents inline. Will probably just be a reduce over the
  body forms checking the first to see if it's 'progn and concat-ing rather
  than cons-ing.

- Add tests for the various grammar fragments
- Fix the existing tests to reflect the changed for behavior (85%)
- Add type table and structure alignment tests

- Add a better pretty-printer now that it is possible to do a namespace based traversal of the symbol table.
- Add a better pretty-printer for code in general.

- Implement a way to dereference a pointer type
- Implement the nested dereference operation required for struct and array access to work as expected.

- Implement a debug library with selectable printing ala debug.h.

- Update the writeup to reflect me.arrdem.compiler.* and me.arrdem.pascal.symtab.*
