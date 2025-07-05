# META
~~~ini
description=equals_with_spaces
type=expr
~~~
# SOURCE
~~~roc
x == y
~~~
# EXPECTED
UNDEFINED VARIABLE - equals_with_spaces.md:1:1:1:2
UNDEFINED VARIABLE - equals_with_spaces.md:1:6:1:7
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpEquals(1:3-1:5),LowerIdent(1:6-1:7),EndOfFile(1:7-1:7),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.7 (op "==")
	(e-ident @1.1-1.2 (raw "x"))
	(e-ident @1.6-1.7 (raw "y")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.7 (op "eq")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "*"))
~~~
