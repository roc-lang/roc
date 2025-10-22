# META
~~~ini
description=Tuple expression
type=expr
~~~
# SOURCE
~~~roc
(1, "hello", True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-int (raw "1"))
	(e-string
		(e-string-part (raw "hello")))
	(e-tag (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-num (value "1"))
		(e-string
			(e-literal (string "hello")))
		(e-tag (name "True"))))
~~~
# TYPES
~~~clojure
(expr (type "(Num(_size), Error, [True]_others)"))
~~~
