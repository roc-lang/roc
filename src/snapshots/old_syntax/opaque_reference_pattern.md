# META
~~~ini
description=opaque_reference_pattern
type=expr
~~~
# SOURCE
~~~roc
when n is
  @Age -> 1
~~~
# EXPECTED
UNDEFINED VARIABLE - opaque_reference_pattern.md:1:1:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
OpaqueName(2:3-2:7),OpArrow(2:8-2:10),Int(2:11-2:12),EndOfFile(2:12-2:12),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
