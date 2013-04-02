# TODO list
- Add tests for the macro system
- Add a type lattice using (https://github.com/jkk/loom)
- Add a way to determine whether a type conversion is required and perform it if so
- Add tests for the various grammar fragments
- Fix the existing tests to reflect the changed for behavior
- Fix the existing tests to remove the deleted parser.clj dep
- Add type table and structure alignment tests

- Rework the way that +, -, * and / generate in the IR, replacing them with
-- M+, M-, M* and M/, being macros which do type checking and argument type conversion before expanding to
-- float+ float- float* float/ int%
-- int+ int- int* int/ int%

- Add a way to enumerate the symbol table ordered by namespace
