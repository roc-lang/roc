# META
~~~ini
description=minus_twelve_minus_five
type=expr
~~~
# SOURCE
~~~roc
-12-5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:4),OpBinaryMinus(1:4-1:5),Int(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.6 (op "-")
	(e-int @1.1-1.4 (raw "-12"))
	(e-int @1.5-1.6 (raw "5")))
~~~
# FORMATTED
~~~roc
-12 - 5
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.6 (op "sub")
	(e-int @1.1-1.4 (value "-12"))
	(e-int @1.5-1.6 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "*"))
~~~
