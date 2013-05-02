# TODO list
- Migrate me.arrdem.pascal.util into me.arrdem.pascal.lexer or .tokens?
- [CRITICAL] add absolute (static heap) allocation for VAR variables. Be able
  to compute the heap address of a value via (addrof).
  -- provide this by making each (program) generate a heap structure of some sort

## back burner
- improve the output of the symbol table printer so as to be... reasonable
  -- make sure that for records it shows the offsets, sizes & fields

- move the "standard" type graph into the standard library symbol table code

- take another look at the lexington toolkit, see if there's a way to
  associate line & column metadata with tokens so that I can print information
  about the failure point rather than the failure and a bunch of garbage.
  -- there is, but I just haven't gone ahead and built it.

- Add tests for the various grammar fragments

- Implement a debug library with selectable printing ala debug.h.

- Take grfrederik's advice and rework the entire symbol table system using the
  old fasioned "pass a map around like nobody's buisness" approach as noted in
  the git log at some point. This will be a major rework and may be difficult
  due to fnparse but it should be done.
