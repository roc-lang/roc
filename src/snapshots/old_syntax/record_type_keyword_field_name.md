# META
~~~ini
description=record_type_keyword_field_name fail
type=expr
~~~
# SOURCE
~~~roc
f : { if : I64 }
~~~
# EXPECTED
UNDEFINED VARIABLE - record_type_keyword_field_name.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),OpenCurly(1:5-1:6),KwIf(1:7-1:9),OpColon(1:10-1:11),UpperIdent(1:12-1:15),CloseCurly(1:16-1:17),EndOfFile(1:17-1:17),
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
