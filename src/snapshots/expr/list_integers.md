# META
~~~ini
description=List with integer literals
type=expr
~~~
# SOURCE
~~~roc
[1, 2, 3]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),Int(1:5-1:6),Comma(1:6-1:7),Int(1:8-1:9),CloseSquare(1:9-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-10
	(e-int @1-2-1-3 (raw "1"))
	(e-int @1-5-1-6 (raw "2"))
	(e-int @1-8-1-9 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-10 (elem-var 78) (id 79)
	(elems
		(e-int @1-2-1-3 (value "1"))
		(e-int @1-5-1-6 (value "2"))
		(e-int @1-8-1-9 (value "3"))))
~~~
# TYPES
~~~clojure
(expr (id 79) (type "List(Num(*))"))
~~~
