# me.arrdem.pascal

This is a pure Clojure implementation of a pascal lexer, parser and symbol
table system. To build from source, perform the following steps:

1. chmod +x ./lein
2. ./lein self-install
3. ./lein uberjar

The jarfile produced by the lein uberjar operation will accept pascal code
either via stdin, or as a file argument. So the invocations
`java -jar target/me.arrdem.pascal-0.1.0-SNAPSHOT-standalone.jar file.pas`
and
`cat file.pas | java -jar target/me.arrdem.pascal-0.1.0-SNAPSHOT-standalone.jar file.pas`
and
`cat file.pas | lein run`
will all invoke the same code which will read the input and attempt to generate
a valid internal representation from it for pretty-printing. 

Unfortunately, if the input is bad this parser/lexer pair will fail absolutely 
with horrific and utterly useless output.

## Output semantics
(comment & vals) 
    these groups are purely for debugging and are ignored

(progn & forms) 
    behaves as a control grouping operation, evaluates subsequent forms.

(goto <integer)
    behaves as one would expect, a goto to a (label) statement elsewhere.

(lable <integer)
    behaves as one would expect, may be the target of (goto)s elsewhere.

(funcall identifier & args)
    behaves as one would expect and invokes a function with a fully qualified 
    name with the trailing arguments.

(:= identifier expr)
    assignment operator, +, -, *, / etc behave the same way.

## Qualified names and the symbol table
This symbol table is based on a hierarchy of nested tables which hide symbols
from lower levels as the semantics of functions, procedures and programs demand.
I present symbols for printing as fully-qualified paths, being a string
<base ns>.<child ns>.<2nd child ns>. ... /<symbol>
so for instance the constant d in graph1 is "toplevel.graph1/d" and soforth.
These semantics are used to note not only variables, but also functions with
functions such as "writeln" being special cases and declared _above_ the top 
level.

For your benifit the compiler is configured to debug the symbol table
_after_ it has dumped the generated IR as a table seperated from the code by a
line of "-" characters.

## License

Copyright © 2013 Reid "arrdem" McKenzie

Distributed under the Eclipse Public License, the same as Clojure.
