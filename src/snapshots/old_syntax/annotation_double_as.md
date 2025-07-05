# META
~~~ini
description=annotation_double_as
type=expr
~~~
# SOURCE
~~~roc
s:(e as A)as A
s
~~~
# EXPECTED
UNDEFINED VARIABLE - annotation_double_as.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),KwAs(1:6-1:8),UpperIdent(1:9-1:10),CloseRound(1:10-1:11),KwAs(1:11-1:13),UpperIdent(1:14-1:15),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "s"))
~~~
# FORMATTED
~~~roc
s
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
