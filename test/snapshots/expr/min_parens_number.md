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
OpUnaryMinus(1:1-1:2),NoSpaceOpenRound(1:2-1:3),Int(1:3-1:4),CloseRound(1:4-1:5),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(unary "-"
	(e-tuple @1.2-1.5
		(e-int @1.3-1.4 (raw "8"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-minus @1.1-1.5
	(e-num @1.3-1.4 (value "8")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Num(_size)"))
~~~
