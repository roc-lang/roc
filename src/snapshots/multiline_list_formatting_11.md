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
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
OpenSquare(2:2-2:3),Int(2:3-2:4),CloseSquare(2:4-2:5),Comma(2:5-2:6),Newline(1:1-1:1),
OpenSquare(3:2-3:3),Int(3:3-3:4),CloseSquare(3:4-3:5),Comma(3:5-3:6),Newline(1:1-1:1),
OpenSquare(4:2-4:3),Newline(1:1-1:1),
Int(5:3-5:4),Comma(5:4-5:5),Newline(1:1-1:1),
Int(6:3-6:4),Comma(6:4-6:5),Newline(1:1-1:1),
CloseSquare(7:2-7:3),Comma(7:3-7:4),Newline(1:1-1:1),
OpenSquare(8:2-8:3),Int(8:3-8:4),CloseSquare(8:4-8:5),Comma(8:5-8:6),Newline(1:1-1:1),
CloseSquare(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-9.2
	(e-list @2.2-2.5
		(e-int @2.3-2.4 (raw "1")))
	(e-list @3.2-3.5
		(e-int @3.3-3.4 (raw "2")))
	(e-list @4.2-7.3
		(e-int @5.3-5.4 (raw "3"))
		(e-int @6.3-6.4 (raw "4")))
	(e-list @8.2-8.5
		(e-int @8.3-8.4 (raw "5"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-9.2
	(elems
		(e-list @2.2-2.5
			(elems
				(e-int @2.3-2.4 (value "1"))))
		(e-list @3.2-3.5
			(elems
				(e-int @3.3-3.4 (value "2"))))
		(e-list @4.2-7.3
			(elems
				(e-int @5.3-5.4 (value "3"))
				(e-int @6.3-6.4 (value "4"))))
		(e-list @8.2-8.5
			(elems
				(e-int @8.3-8.4 (value "5"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-9.2 (type "List(List(Num(*)))"))
~~~
