# META
~~~ini
description=basic_tuple
type=expr
~~~
# SOURCE
~~~roc
(1, 2, 3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),Int(1:8-1:9),CloseRound(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.10
	(e-int @1.2-1.3 (raw "1"))
	(e-int @1.5-1.6 (raw "2"))
	(e-int @1.8-1.9 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.10
	(elems
		(e-int @1.2-1.3 (value "1"))
		(e-int @1.5-1.6 (value "2"))
		(e-int @1.8-1.9 (value "3"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.10 (type "(Num(*), Num(*), Num(*))"))
~~~
