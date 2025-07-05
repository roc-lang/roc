# META
~~~ini
description=newline_singleton_list
type=expr
~~~
# SOURCE
~~~roc
[
1
]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
Int(2:1-2:2),Newline(1:1-1:1),
CloseSquare(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-3.2
	(e-int @2.1-2.2 (raw "1")))
~~~
# FORMATTED
~~~roc
[
	1,
]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-3.2
	(elems
		(e-int @2.1-2.2 (value "1"))))
~~~
# TYPES
~~~clojure
(expr @1.1-3.2 (type "List(Num(*))"))
~~~
