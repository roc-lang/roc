# META
~~~ini
description=Empty list unifies with non-empty list
type=expr
~~~
# SOURCE
~~~roc
[[], [1, 2, 3]]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),CloseSquare(1:3-1:4),Comma(1:4-1:5),OpenSquare(1:6-1:7),Int(1:7-1:8),Comma(1:8-1:9),Int(1:10-1:11),Comma(1:11-1:12),Int(1:13-1:14),CloseSquare(1:14-1:15),CloseSquare(1:15-1:16),EndOfFile(1:16-1:16),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-16
	(e-list @1-2-1-4)
	(e-list @1-6-1-15
		(e-int @1-7-1-8 (raw "1"))
		(e-int @1-10-1-11 (raw "2"))
		(e-int @1-13-1-14 (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-16 (elem-var 79) (id 80)
	(elems
		(e-list @1-2-1-4 (elem-var 72)
			(elems))
		(e-list @1-6-1-15 (elem-var 77)
			(elems
				(e-int @1-7-1-8 (value "1"))
				(e-int @1-10-1-11 (value "2"))
				(e-int @1-13-1-14 (value "3"))))))
~~~
# TYPES
~~~clojure
(expr (id 80) (type "List(List(Num(*)))"))
~~~