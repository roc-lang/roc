# META
~~~ini
description=Nested list literals
type=expr
~~~
# SOURCE
~~~roc
[[1, 2], [3, 4], [5]]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),OpenSquare(1:2-1:3),Int(1:3-1:4),Comma(1:4-1:5),Int(1:6-1:7),CloseSquare(1:7-1:8),Comma(1:8-1:9),OpenSquare(1:10-1:11),Int(1:11-1:12),Comma(1:12-1:13),Int(1:14-1:15),CloseSquare(1:15-1:16),Comma(1:16-1:17),OpenSquare(1:18-1:19),Int(1:19-1:20),CloseSquare(1:20-1:21),CloseSquare(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-list @1-1-1-22
	(e-list @1-2-1-8
		(e-int @1-3-1-4 (raw "1"))
		(e-int @1-6-1-7 (raw "2")))
	(e-list @1-10-1-16
		(e-int @1-11-1-12 (raw "3"))
		(e-int @1-14-1-15 (raw "4")))
	(e-list @1-18-1-21
		(e-int @1-19-1-20 (raw "5"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list @1-1-1-22 (elem-var 93) (id 94)
	(elems
		(e-list @1-2-1-8 (elem-var 78)
			(elems
				(e-int @1-3-1-4 (int-var 73) (precision-var 72) (literal "1") (value "TODO") (bound "u8"))
				(e-int @1-6-1-7 (int-var 76) (precision-var 75) (literal "2") (value "TODO") (bound "u8"))))
		(e-list @1-10-1-16 (elem-var 86)
			(elems
				(e-int @1-11-1-12 (int-var 81) (precision-var 80) (literal "3") (value "TODO") (bound "u8"))
				(e-int @1-14-1-15 (int-var 84) (precision-var 83) (literal "4") (value "TODO") (bound "u8"))))
		(e-list @1-18-1-21 (elem-var 91)
			(elems
				(e-int @1-19-1-20 (int-var 89) (precision-var 88) (literal "5") (value "TODO") (bound "u8"))))))
~~~
# TYPES
~~~clojure
(expr (id 94) (type "List(List(Num(Int(*))))"))
~~~