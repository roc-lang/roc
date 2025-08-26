# META
~~~ini
description=Tuple as an argument
type=expr
~~~
# SOURCE
~~~roc
(|(x,y)| x * y )((1,2))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),Comma(1:5-1:6),LowerIdent(1:6-1:7),CloseRound(1:7-1:8),OpBar(1:8-1:9),LowerIdent(1:10-1:11),OpStar(1:12-1:13),LowerIdent(1:14-1:15),CloseRound(1:16-1:17),NoSpaceOpenRound(1:17-1:18),NoSpaceOpenRound(1:18-1:19),Int(1:19-1:20),Comma(1:20-1:21),Int(1:21-1:22),CloseRound(1:22-1:23),CloseRound(1:23-1:24),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.24
	(e-tuple @1.1-1.17
		(e-lambda @1.2-1.15
			(args
				(p-tuple @1.3-1.8
					(p-ident @1.4-1.5 (raw "x"))
					(p-ident @1.6-1.7 (raw "y"))))
			(e-binop @1.10-1.15 (op "*")
				(e-ident @1.10-1.11 (raw "x"))
				(e-ident @1.14-1.15 (raw "y")))))
	(e-tuple @1.18-1.23
		(e-int @1.19-1.20 (raw "1"))
		(e-int @1.21-1.22 (raw "2"))))
~~~
# FORMATTED
~~~roc
(|(x, y)| x * y)((1, 2))
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.24
	(e-lambda @1.2-1.15
		(args
			(p-tuple @1.3-1.8
				(patterns
					(p-assign @1.4-1.5 (ident "x"))
					(p-assign @1.6-1.7 (ident "y")))))
		(e-binop @1.10-1.15 (op "mul")
			(e-lookup-local @1.10-1.11
				(p-assign @1.4-1.5 (ident "x")))
			(e-lookup-local @1.14-1.15
				(p-assign @1.6-1.7 (ident "y")))))
	(e-tuple @1.18-1.23
		(elems
			(e-int @1.19-1.20 (value "1"))
			(e-int @1.21-1.22 (value "2")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.24 (type "Num(_size)"))
~~~
