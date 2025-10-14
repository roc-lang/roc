# META
~~~ini
description=multiline_list_formatting (14)
type=expr
~~~
# SOURCE
~~~roc
[ # Open
	1, # First

	# A comment in the middle

	2, # Second
	# This comment has no blanks around it
	3, # Third
]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),
Int(2:2-2:3),Comma(2:3-2:4),
Int(6:2-6:3),Comma(6:3-6:4),
Int(8:2-8:3),Comma(8:3-8:4),
CloseSquare(9:1-9:2),
EndOfFile(10:1-10:1),
~~~
# PARSE
~~~clojure
(e-list @1.1-9.2
	(e-int @2.2-2.3 (raw "1"))
	(e-int @6.2-6.3 (raw "2"))
	(e-int @8.2-8.3 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-9.2
	(elems
		(e-num @2.2-2.3 (value "1"))
		(e-num @6.2-6.3 (value "2"))
		(e-num @8.2-8.3 (value "3"))))
~~~
# TYPES
~~~clojure
(expr @1.1-9.2 (type "List(Num(_size))"))
~~~
