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
Int,
OpPlus,
Int,
OpStar,
Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "+")
	(e-int (raw "1"))
	(e-binop (op "*")
		(e-int (raw "2"))
		(e-int (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-binop (op "add")
	(e-num (value "1"))
	(e-binop (op "mul")
		(e-num (value "2"))
		(e-num (value "3"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
