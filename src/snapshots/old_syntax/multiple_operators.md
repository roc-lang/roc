# META
~~~ini
description=multiple_operators
type=expr
~~~
# SOURCE
~~~roc
31*42+534
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:3),OpStar(1:3-1:4),Int(1:4-1:6),OpPlus(1:6-1:7),Int(1:7-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-binop @1.1-1.10 (op "+")
	(e-binop @1.1-1.7 (op "*")
		(e-int @1.1-1.3 (raw "31"))
		(e-int @1.4-1.6 (raw "42")))
	(e-int @1.7-1.10 (raw "534")))
~~~
# FORMATTED
~~~roc
31 * 42 + 534
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.10 (op "add")
	(e-binop @1.1-1.7 (op "mul")
		(e-int @1.1-1.3 (value "31"))
		(e-int @1.4-1.6 (value "42")))
	(e-int @1.7-1.10 (value "534")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "a"))
~~~
