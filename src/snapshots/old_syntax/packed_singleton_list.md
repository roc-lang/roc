# META
~~~ini
description=packed_singleton_list
type=expr
~~~
# SOURCE
~~~roc
[1]
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),CloseSquare(1:3-1:4),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-list @1.1-1.4
	(e-int @1.2-1.3 (raw "1")))
~~~
# FORMATTED
~~~roc
[1]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-1.4
	(elems
		(e-int @1.2-1.3 (value "1"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "List(Num(*))"))
~~~
