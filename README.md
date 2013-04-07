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
("comment" & vals)
    these groups are purely for debugging and are ignored

("progn" & forms)
    behaves as a control grouping operation, evaluates subsequent forms.

("goto" <integer>)
    behaves as one would expect, a goto to a (label) statement elsewhere.

("label" <integer>)
    behaves as one would expect, may be the target of (goto)s elsewhere.

("funcall" <identifier> <& args>)
    behaves as one would expect and invokes a function with a fully qualified
    name with the trailing arguments.

(":=" <identifier> <expr>)
    assignment operator, "+", "-", "*", "/" etc behave the same way.

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

## Source code
The source for this project is located under src/, the following is a quick list
of file names and rough descriptions.

#### src/me/arrdem/pascal.clj
Contains the main method & formatting code

#### src/me/arrdem/pascal/grammar.clj
Defines the grammar of pascal in terms of tokens, I'm proud to say that this
grammar was programaticly generated using the latest build of the sad toolkit
 which I posted on the piazza page some time ago.

#### src/me/arrdem/pascal/semantics.clj
Defines a large number of semantic transformations on the grammar which the
grammar file defines, used to generate the IR which is pretty-printed and
to incur side-effects such as symbol table manipulation.

#### src/me/arrdem/pascal/ast.clj
Defines some AST manipulation routines which are used to help keep the semantics 
file clean, readable and abstract with respect to the implementation of the 
funal AST structure.

#### src/me/arrdem/pascal/lexer.clj
Defines the lexer in terms of some transforms atop the tokens defined in 
tokens.clj.

#### src/me/arrdem/pascal/tokens.clj
Defines the basic tokens for the pascal lexer. Via a macro in util, also
defines rules used by the grammar for matching these tokens.

#### src/me/arrdem/pascal/util.clj
Holding pen for code with no better home as of yet.

#### src/me/arrdem/pascal/symtab.clj
Defines the symbol table and the various legal operations therupon such as
install! and search. As of 0.2.0 backed largely by code in
me.arrdem.compiler.

#### src/me/arrdem/compiler/*
Various elements of the compiler largely related to the symbol table, type
and macro systems which I am deliberately over-engineering with the intent
of building a serious Lisp compiler after the end of this course. For this 
reason they have been abstracted out and seperated in a differnet directory tree 
so that they can later be packaged as their own library should it prove 
appropriate.

## License

Copyright © 2013 Reid "arrdem" McKenzie

Distributed under the Eclipse Public License, the same as Clojure.
