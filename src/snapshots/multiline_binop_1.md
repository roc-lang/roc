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
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),Newline(1:4-1:8),
OpPlus(2:2-2:3),Newline(2:5-2:10),
Newline(1:1-1:1),
Newline(4:3-4:24),
Newline(1:1-1:1),
Int(6:2-6:3),Newline(6:5-6:9),
OpStar(7:3-7:4),Newline(7:6-7:12),
Int(8:3-8:4),EndOfFile(8:4-8:4),
~~~
# PARSE
~~~clojure
(e-binop @1-1-8-4 (op "+")
	(e-int @1-1-1-2 (raw "1"))
	(e-binop @6-2-8-4 (op "*")
		(e-int @6-2-6-3 (raw "2"))
		(e-int @8-3-8-4 (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop @1-1-8-4 (op "add") (id 79)
	(e-int @1-1-1-2 (num-var 73) (value "1"))
	(e-binop @6-2-8-4 (op "mul")
		(e-int @6-2-6-3 (num-var 75) (value "2"))
		(e-int @8-3-8-4 (num-var 77) (value "3"))))
~~~
# TYPES
~~~clojure
(expr (id 79) (type "*"))
~~~