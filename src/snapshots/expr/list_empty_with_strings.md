# META
~~~ini
description=Empty list unifies with string list
type=expr
~~~
# SOURCE
~~~roc
[[], ["hello", "world"]]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),CloseSquare(1:3-1:4),Comma(1:4-1:5),OpenSquare(1:6-1:7),StringStart(1:7-1:8),StringPart(1:8-1:13),StringEnd(1:13-1:14),Comma(1:14-1:15),StringStart(1:16-1:17),StringPart(1:17-1:22),StringEnd(1:22-1:23),CloseSquare(1:23-1:24),CloseSquare(1:24-1:25),EndOfFile(1:25-1:25),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-25
	(e-list @1-2-1-4)
	(e-list @1-6-1-24
		(e-string @1-7-1-14
			(e-string-part @1-8-1-13 (raw "hello")))
		(e-string @1-16-1-23
			(e-string-part @1-17-1-22 (raw "world")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-25 (elem-var 80) (id 81)
	(elems
		(e-list @1-2-1-4 (elem-var 72)
			(elems))
		(e-list @1-6-1-24 (elem-var 78)
			(elems
				(e-string @1-7-1-14
					(e-literal @1-8-1-13 (string "hello")))
				(e-string @1-16-1-23
					(e-literal @1-17-1-22 (string "world")))))))
~~~
# TYPES
~~~clojure
(expr (id 81) (type "List(List(Str))"))
~~~