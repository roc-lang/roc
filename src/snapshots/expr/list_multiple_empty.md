# META
~~~ini
description=Multiple empty lists unify together
type=expr
~~~
# SOURCE
~~~roc
[[], [], []]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),CloseSquare(1:3-1:4),Comma(1:4-1:5),OpenSquare(1:6-1:7),CloseSquare(1:7-1:8),Comma(1:8-1:9),OpenSquare(1:10-1:11),CloseSquare(1:11-1:12),CloseSquare(1:12-1:13),EndOfFile(1:13-1:13),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-13
	(e-list @1-2-1-4)
	(e-list @1-6-1-8)
	(e-list @1-10-1-12))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-13 (elem-var 78) (id 79)
	(elems
		(e-list @1-2-1-4 (elem-var 72)
			(elems))
		(e-list @1-6-1-8 (elem-var 74)
			(elems))
		(e-list @1-10-1-12 (elem-var 76)
			(elems))))
~~~
# TYPES
~~~clojure
(expr (id 79) (type "List(List(*))"))
~~~