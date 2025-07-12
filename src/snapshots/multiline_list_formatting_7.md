# META
~~~ini
description=multiline_list_formatting (7)
type=expr
~~~
# SOURCE
~~~roc
[
	1,
	2, # Foo
	3,
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
Int(3:2-3:3),Comma(3:3-3:4),
Int(4:2-4:3),Comma(4:3-4:4),
CloseSquare(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-5.2
	(e-int @2.2-2.3 (raw "1"))
	(e-int @3.2-3.3 (raw "2"))
	(e-int @4.2-4.3 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-5.2
	(elems
		(e-int @2.2-2.3 (value "1"))
		(e-int @3.2-3.3 (value "2"))
		(e-int @4.2-4.3 (value "3"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "List(Num(*))"))
~~~
