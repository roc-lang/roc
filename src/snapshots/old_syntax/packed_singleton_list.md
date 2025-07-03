# META
~~~ini
description=packed_singleton_list
type=expr
~~~
# SOURCE
~~~roc
[1]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),CloseSquare(1:3-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.4
	(e-int @1.2-1.3 (raw "1")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.4
	(elems
		(e-int @1.2-1.3 (value "1"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "List(Num(a))"))
~~~
