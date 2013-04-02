# TODO list
- Add tests for the macro system
- Add a type lattice using (https://github.com/jkk/loom)
- Add a way to determine whether a type conversion is required and perform it if so
  -- it seems to me that the easiest thing to do would be to recur from the left, taking
     the type of the leftmost sub-expression and asserting that any type T encountered is
     higher or equal to that base type converting where appropriate. As type conversion
     will have already taken place, then code generation is trivial because type is ensured.
     This should be a macro which is applied prior to code generation.
- Rework the way that +, -, * and / generate in the IR, replacing them with
  -- M+, M-, M* and M/, being macros which do type checking and argument type conversion before expanding to
  -- float+ float- float* float/ int%
  -- int+ int- int* int/ int%

- Add tests for the various grammar fragments
- Fix the existing tests to reflect the changed for behavior
- Fix the existing tests to remove the deleted parser.clj dep
- Add type table and structure alignment tests

- Add a way to enumerate the symbol table ordered by namespace
