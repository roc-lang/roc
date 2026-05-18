# META
~~~ini
description=Chained tuple access
type=expr
~~~
# SOURCE
~~~roc
((1, 2), (3, 4)).0.1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,Comma,OpenRound,Int,Comma,Int,CloseRound,CloseRound,NoSpaceDotInt,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-tuple-access
		(e-tuple
			(e-tuple
				(e-int (raw "1"))
				(e-int (raw "2")))
			(e-tuple
				(e-int (raw "3"))
				(e-int (raw "4"))))
		".0")
	".1")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "1")
	(e-tuple-access (index "0")
		(e-tuple
			(elems
				(e-tuple
					(elems
						(e-num (value "1"))
						(e-num (value "2"))))
				(e-tuple
					(elems
						(e-num (value "3"))
						(e-num (value "4"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
