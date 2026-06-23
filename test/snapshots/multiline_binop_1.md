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
(e-dispatch-call (method "plus") (constraint-fn-var 112)
	(receiver
		(e-num (value "1")))
	(args
		(e-dispatch-call (method "times") (constraint-fn-var 110)
			(receiver
				(e-num (value "2")))
			(args
				(e-num (value "3"))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
