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
(e-unary-minus
	(e-num (value "8")))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.negate : a -> a]"))
~~~
