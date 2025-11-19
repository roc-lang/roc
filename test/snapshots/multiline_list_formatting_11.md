# META
~~~ini
description=multiline_list_formatting (11)
type=expr
~~~
# SOURCE
~~~roc
[
	[1],
	[2],
	[
		3,
		4,
	],
	[5],
]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare,
OpenSquare,Int,CloseSquare,Comma,
OpenSquare,Int,CloseSquare,Comma,
OpenSquare,
Int,Comma,
Int,Comma,
CloseSquare,Comma,
OpenSquare,Int,CloseSquare,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-list
		(e-int (raw "1")))
	(e-list
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
				(e-num (value "1"))))
		(e-list
			(elems
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
