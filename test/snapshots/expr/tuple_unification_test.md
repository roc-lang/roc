# META
~~~ini
description=Tuple unification with different types
type=expr
~~~
# SOURCE
~~~roc
[(1, "a"), (2.5, "b")]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,Comma,OpenRound,Float,Comma,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-tuple
		(e-int (raw "1"))
		(e-string
			(e-string-part (raw "a"))))
	(e-tuple
		(e-frac (raw "2.5"))
		(e-string
			(e-string-part (raw "b")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-string
					(e-literal (string "a")))))
		(e-tuple
			(elems
				(e-dec-small (numerator "25") (denominator-power-of-ten "1") (value "2.5"))
				(e-string
					(e-literal (string "b")))))))
~~~
# TYPES
~~~clojure
(expr (type "List((Num(Frac(_size)), Error))"))
~~~
