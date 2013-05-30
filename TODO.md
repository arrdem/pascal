# TODO list
- Migrate me.arrdem.pascal.util into me.arrdem.pascal.lexer or .tokens?
- [CRITICAL] add absolute (static heap) allocation for VAR variables. Be able
  to compute the heap address of a value via (addrof).
  -- provide this by making each (program) generate a heap structure of some sort
- [CRITICAL] get code generation working
  -- register spilling, figure out how to build an LRU system
  -- figure out converting floats to ints and back
- Implement better command line arguments
  -- macros? (defaults to true)

- Rework anon type naming so that it's both deterministic and repeatable. EG:
  two structs with the same sequence of member fields should generate a named 
  ThinType and use the _same_ anon backend type. Also two arrays of the same
  dimensions & types should resolve to the same backed generated type.

## back burner
- improve the output of the symbol table printer so as to be... reasonable
  -- make sure that for records it shows the offsets, sizes & fields

- move the "standard" type graph into the standard library symbol table code

- take another look at the lexington toolkit, see if there's a way to
  associate line & column metadata with tokens so that I can print information
  about the failure point rather than the failure and a bunch of garbage.
  -- there is, but I just haven't gone ahead and built it.

- Add tests for the various grammar fragments

- Instrument via the Java logging library (clojure.tools.logging)

- Take grfrederik's advice and rework the entire symbol table system using the
  old fashioned "pass a map around like nobody's buisness" approach as noted in
  the git log at some point. This will be a major rework and may be difficult
  due to fnparse but it should be done.
