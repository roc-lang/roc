# META
~~~ini
description=spaced_singleton_list
type=expr
~~~
# SOURCE
~~~roc
[ 1 ]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:3-1:4),CloseSquare(1:5-1:6),EndOfFile(1:6-1:6),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.6
	(e-int @1.3-1.4 (raw "1")))
~~~
# FORMATTED
~~~roc
[1]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.6
	(elems
		(e-int @1.3-1.4 (value "1"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.6 (type "List(Num(*))"))
~~~
