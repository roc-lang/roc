# META
~~~ini
description=add_var_with_spaces
type=expr
~~~
# SOURCE
~~~roc
x + 2
~~~
# EXPECTED
UNDEFINED VARIABLE - add_var_with_spaces.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpPlus(1:3-1:4),Int(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.6 (op "+")
	(e-ident @1.1-1.2 (raw "x"))
	(e-int @1.5-1.6 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "add")
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-int @1.5-1.6 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "*"))
~~~
