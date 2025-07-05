# META
~~~ini
description=space_only_after_minus
type=expr
~~~
# SOURCE
~~~roc
x- y
~~~
# EXPECTED
UNDEFINED VARIABLE - space_only_after_minus.md:1:1:1:2
UNDEFINED VARIABLE - space_only_after_minus.md:1:4:1:5
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpBinaryMinus(1:2-1:3),LowerIdent(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.5 (op "-")
	(e-ident @1.1-1.2 (raw "x"))
	(e-ident @1.4-1.5 (raw "y")))
~~~
# FORMATTED
~~~roc
x - y
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.5 (op "sub")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "*"))
~~~
