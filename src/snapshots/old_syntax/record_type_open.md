# META
~~~ini
description=record_type_open fail
type=expr
~~~
# SOURCE
~~~roc
f : {
~~~
# EXPECTED
UNDEFINED VARIABLE - record_type_open.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "f"))
~~~
# FORMATTED
~~~roc
f
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
