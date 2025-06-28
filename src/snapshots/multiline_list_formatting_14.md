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
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:4-1:9),
Int(2:2-2:3),Comma(2:3-2:4),Newline(2:6-2:12),
Newline(1:1-1:1),
Newline(4:3-4:27),
Newline(1:1-1:1),
Int(6:2-6:3),Comma(6:3-6:4),Newline(6:6-6:13),
Newline(7:3-7:40),
Int(8:2-8:3),Comma(8:3-8:4),Newline(8:6-8:12),
CloseSquare(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(e-list @1-1-9-2
	(e-int @2-2-2-3 (raw "1"))
	(e-int @6-2-6-3 (raw "2"))
	(e-int @8-2-8-3 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-9-2 (elem-var 75) (id 76)
	(elems
		(e-int @2-2-2-3 (value "1"))
		(e-int @6-2-6-3 (value "2"))
		(e-int @8-2-8-3 (value "3"))))
~~~
# TYPES
~~~clojure
(expr (id 76) (type "List(Num(*))"))
~~~