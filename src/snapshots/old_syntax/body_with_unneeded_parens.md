# META
~~~ini
description=body_with_unneeded_parens
type=expr
~~~
# SOURCE
~~~roc
a=(
6)
a
~~~
# EXPECTED
UNDEFINED VARIABLE - body_with_unneeded_parens.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
Int(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
