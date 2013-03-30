# TODO list
- Add tests for the macro system
- Add type conversion (int->real ) functions to the standard symbol table
- Add a type lattice using (https://github.com/jkk/loom)
v- Add tests for the various grammar fragments
- Fix the existing tests to reflect the changed for behavior
- Fix the existing tests to remove the deleted parser.clj dep
- Rework the way that +, -, * and / generate in the IR, replacing them with
-- M+, M-, M* and M/, being macros which do type checking and argument type conversion before expanding to
-- float+ float- float* float/ int%
-- int+ int- int* int/ int%
- Add a way to look up a fully qualified name in the symbol table directly
- Add a way to enumerate the symbol table ordered by namespace
- Think about & comment up how to execute code generation... probably want the symbol table to embed a second suite of macros which do code gen but which are accessed using a different key so that we have a macro time and a code gen time
