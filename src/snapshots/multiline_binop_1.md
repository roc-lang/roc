# META
~~~ini
description=multiline_binop (1)
type=expr
~~~
# SOURCE
~~~roc
1 # One
	+ # Plus

	# A comment in between

	2 # Two
		* # Times
		3
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),
OpPlus(2:2-2:3),
Int(6:2-6:3),
OpStar(7:3-7:4),
Int(8:3-8:4),EndOfFile(8:4-8:4),
~~~
# PARSE
~~~clojure
(e-binop @1.1-8.4 (op "+")
	(e-int @1.1-1.2 (raw "1"))
	(e-binop @6.2-8.4 (op "*")
		(e-int @6.2-6.3 (raw "2"))
		(e-int @8.3-8.4 (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1.1-8.4 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-binop @6.2-8.4 (op "mul")
		(e-int @6.2-6.3 (value "2"))
		(e-int @8.3-8.4 (value "3"))))
~~~
# TYPES
~~~clojure
(expr @1.1-8.4 (type "a"))
~~~
