# me.arrdem.pascal

This is a pure Clojure implementation of a pascal lexer, parser and symbol
table system. To build from source, perform the following steps:

1. chmod +x ./lein
2. ./lein self-install
3. ./lein uberjar

The jarfile produced by the lein uberjar operation will accept pascal code
either via stdin, or as a file argument. I sugest the invocation
`cat file.pas | lein run`
but
`java -jar target/me.arrdem.pascal-0.1.0-SNAPSHOT-standalone.jar file.pas`
and
`cat file.pas | java -jar target/me.arrdem.pascal-0.1.0-SNAPSHOT-standalone.jar file.pas`
will all invoke the same code which will read the input and attempt to generate
a valid internal representation from it for pretty-printing.

Unfortunately, if the input is bad this parser/lexer pair will fail absolutely
with horrific and utterly useless output.

## Output semantics
(comment & vals)
    these groups are purely for debugging and are ignored

(progn & forms)
    behaves as a control grouping operation, evaluates subsequent forms.

(goto <integer>)
    behaves as one would expect, a goto to a (label) statement elsewhere.

(label <integer>)
    behaves as one would expect, may be the target of (goto)s elsewhere.

(funcall <identifier> & <args>)
    behaves as one would expect and invokes a function with a fully qualified
    name with the trailing arguments.

(:= <identifier> <expr>)
    assignment operator, "+", "-", "*", "/" etc behave the same way.

(<type>-><type> <symbol>)
    the compiler generates "->" functions which convert between adjacent types
    in the type graph defined in me.arrdem.pascal.types. when type coercion is
    on the cards uses of these functions are generated automatically.

## Qualified names and the symbol table
This symbol table is based on a hierarchy of nested tables which hide symbols
from lower levels as the semantics of functions, procedures and programs demand.
I present symbols for printing as fully-qualified paths, being a string
<base ns>.<child ns>.<2nd child ns>. ... /<symbol>
so for instance the constant d in graph1 is "graph1/d" and soforth.
These semantics are used to note not only variables, but also functions with
functions such as "writeln" being special cases and declared _at_ the top
level.

For your benifit the compiler is configured to debug the symbol table
_after_ it has dumped the generated IR as a table seperated from the code by a
line of "-" characters.

Please note that strings and literal values are replaced with generated symbols.
The string " " for instance is likely to be given a name such as
graph1/string_0. Because we will eventually have to compute a data
segment, I have take the preemptive measure of interning strings and all other
constants in the symbol table for later optimization or simple inclusion. This
behavior also holds for literals such as 1 and 32, for which integer variables
with explicit values are created. Note that constants are typed to be refs to
these generated variables rather than hold the value in and of themselves.

## Fun & extra stuff
I implement a few kinda cute extras which I feel bear mentioning. First of all
I provide a full graph based type system which supports arbitrary nesting of
types, pointers and structures while remaining printable throughout thanks to
an "ISymbol" protocol which I define in me.arrdem.compiler and extend to my
internal symbol representation in me.arrdem.compiler.symbols and even to
strings and some generic objects in me.arrdem.compiler.symbol_conversions.

Second off and perhaps most frustrating to implement I provide a full Lisp
style unhygenic untyped syntax based macro system. Macros are treated as first
class symbol table citizens and are installed along with the rest of the
"standard library" from me.arrdem.pascal.symtab.*. At present I provide macros
to optimize out nested progn groups, provide type to byte conversion for
invocations of the "new" function. Depending on when I submit you may or may
not find that I have also implemented nested aref optimization as well as
partial evaluation & elimination of arithmetic expressions via the macro
system.

It is my intent to base code generation entirely upon the macro system when
the time comes for that.

Some significant effort has been made to clean up the codebase for readability
and in favor of idiomatic approaches such as use of first class functions and
the -> macro to eliminate nesting of expressions. The real benefit is now
near-universal commenting of files & functions.

## Source code
The source for this project is located under src/, the following is a quick list
of major file names and rough descriptions. More information is available in the 
commets & namespace documentation at the head of most files.

    .
    ├── TODO.md     <- my todo list
    ├── lein        <- leiningen binary
    ├── project.clj <- leiningen project metadata
    ├── README.md   <- you are here
    ├── src
    │   └── me
    │       └── arrdem
    │           ├── compiler.clj <- defines interfaces related to the type matrix
    │           │                   which are heavily used elsewhere in the codebase.
    │           ├── pascal.clj <- defines the main function and does some pretty
    │           │                 printing work for the sake of the TA and debugging
    │           ├── compiler
    │           │   ├── macros.clj <- implements a full macro system
    │           │   ├── symbols.clj <- provides a number of record (class) types which
    │           │   │                  extend the interfaces defined in compiler.clj
    │           │   ├── symbol_conversions.clj
    │           │   │              <- extends those interfaces to basic
    │           │   │                 Clojure and Java types in order to provide
    │           │   │                 a near invisible symbol table interface
    │           │   │                 which "just works" on symbol names & types.
    │           │   ├── symtab.clj <- implements a symbol table
    │           │   └── types.clj  <- implements parts of a type hierarchy
    │           └── pascal
    │               ├── ast.clj       <- some generics for manipulating and building
    │               │                    my internal AST representation.
    │               ├── grammar.clj   <- defines the pascal grammar productions and
    │               │                    corresponding ast generators in
    │               │                    semantics.clj
    │               ├── semantics.clj <- defines token -> AST productions invoked by
    │               │                    grammar.clj in terms of ast.clj &
    │               │                    symtab.clj
    │               ├── tokens.clj    <- defines the tokens which comprise the Pascal
    │               │                    language, used to create the lexer in
    │               │                    lexer.clj
    │               ├── lexer.clj     <- uses util.clj and tokens.clj to build a
    │               │                    pascal token lexer.
    │               ├── types.clj     <- wrapper around compiler/types.clj which
    │               │                    adds the type matrix and some other
    │               │                    pascal-specific functionality.
    │               ├── symtab.clj    <- wrapper around compiler/symtab.clj, which
    │               │                    among other things builds the standard
    │               │                    symbol table using the files in symtab/
    │               ├── symtab        <- contains various
    │               │   ├── stdlib.clj    <- std lib function defs.
    │               │   ├── stdmacros.clj <- std lib macro defs.
    │               │   └── stdtypes.clj  <- std lib type defs.
    │               └── util.clj      <- junk that didn't go somewhere else
    └── test <tree out of date>
        └── me
            └── arrdem
                ├── compiler
                │   └── symab_tests.clj
                └── pascal
                    ├── core_test.clj
                    ├── lexer_test.clj
                    ├── test_text.clj
                    └── types_test.clj

## License

Copyright © 2013 Reid "arrdem" McKenzie

Distributed under the Eclipse Public License, the same as Clojure.
