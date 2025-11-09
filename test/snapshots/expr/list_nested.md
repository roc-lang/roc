# META
~~~ini
description=Nested list literals
type=expr
~~~
# SOURCE
~~~roc
[[1, 2], [3, 4], [5]]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare,OpenSquare,Int,Comma,Int,CloseSquare,Comma,OpenSquare,Int,Comma,Int,CloseSquare,Comma,OpenSquare,Int,CloseSquare,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list
		(e-int (raw "1"))
		(e-int (raw "2")))
	(e-list
		(e-int (raw "3"))
		(e-int (raw "4")))
	(e-list
		(e-int (raw "5"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))))
		(e-list
			(elems
				(e-num (value "3"))
				(e-num (value "4"))))
		(e-list
			(elems
				(e-num (value "5"))))))
~~~
# TYPES
~~~clojure
(expr (type "List(List(Num(_size)))"))
~~~
