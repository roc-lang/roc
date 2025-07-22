# META
~~~ini
description=Basic lambda capture detection during canonicalization
type=expr
~~~
# SOURCE
~~~roc
(|x| |y| x + y)(1)(2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),OpBar(1:4-1:5),OpBar(1:6-1:7),LowerIdent(1:7-1:8),OpBar(1:8-1:9),LowerIdent(1:10-1:11),OpPlus(1:12-1:13),LowerIdent(1:14-1:15),CloseRound(1:15-1:16),NoSpaceOpenRound(1:16-1:17),Int(1:17-1:18),CloseRound(1:18-1:19),NoSpaceOpenRound(1:19-1:20),Int(1:20-1:21),CloseRound(1:21-1:22),EndOfFile(1:22-1:22),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.22
	(e-apply @1.1-1.19
		(e-tuple @1.1-1.16
			(e-lambda @1.2-1.15
				(args
					(p-ident @1.3-1.4 (raw "x")))
				(e-lambda @1.6-1.15
					(args
						(p-ident @1.7-1.8 (raw "y")))
					(e-binop @1.10-1.15 (op "+")
						(e-ident @1.10-1.11 (raw "x"))
						(e-ident @1.14-1.15 (raw "y"))))))
		(e-int @1.17-1.18 (raw "1")))
	(e-int @1.20-1.21 (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.22
	(e-call @1.1-1.19
		(e-lambda @1.2-1.15
			(args
				(p-assign @1.3-1.4 (ident "x")))
			(e-lambda @1.6-1.15
				(args
					(p-assign @1.7-1.8 (ident "y")))
				(captures
					(capture @1.3-1.4 (ident "x")))
				(e-binop @1.10-1.15 (op "add")
					(e-lookup-local @1.10-1.11
						(p-assign @1.3-1.4 (ident "x")))
					(e-lookup-local @1.14-1.15
						(p-assign @1.7-1.8 (ident "y"))))))
		(e-int @1.17-1.18 (value "1")))
	(e-int @1.20-1.21 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.22 (type "Num(_size)"))
~~~
