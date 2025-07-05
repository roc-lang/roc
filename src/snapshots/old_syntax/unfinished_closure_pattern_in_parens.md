# META
~~~ini
description=unfinished_closure_pattern_in_parens fail
type=expr
~~~
# SOURCE
~~~roc
x = \( a
)
~~~
# EXPECTED
UNDEFINED VARIABLE - unfinished_closure_pattern_in_parens.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),OpBackslash(1:5-1:6),NoSpaceOpenRound(1:6-1:7),LowerIdent(1:8-1:9),Newline(1:1-1:1),
CloseRound(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "x"))
~~~
# FORMATTED
~~~roc
x
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
