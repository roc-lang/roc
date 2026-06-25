# META
~~~ini
description=min_parens_number
type=expr
~~~
# SOURCE
~~~roc
-(8)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpUnaryMinus,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "-"
	(e-tuple
		(e-int (raw "8"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dispatch-call (method "negate") (constraint-fn-var 41)
	(receiver
		(e-num (value "8")))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
