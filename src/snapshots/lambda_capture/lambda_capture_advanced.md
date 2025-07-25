# META
~~~ini
description=More davanced lambda capture
type=expr
~~~
# SOURCE
~~~roc
(|a, b, c| |x| a + b + c + x)(10, 20, 5)(7)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),Comma(1:4-1:5),LowerIdent(1:6-1:7),Comma(1:7-1:8),LowerIdent(1:9-1:10),OpBar(1:10-1:11),OpBar(1:12-1:13),LowerIdent(1:13-1:14),OpBar(1:14-1:15),LowerIdent(1:16-1:17),OpPlus(1:18-1:19),LowerIdent(1:20-1:21),OpPlus(1:22-1:23),LowerIdent(1:24-1:25),OpPlus(1:26-1:27),LowerIdent(1:28-1:29),CloseRound(1:29-1:30),NoSpaceOpenRound(1:30-1:31),Int(1:31-1:33),Comma(1:33-1:34),Int(1:35-1:37),Comma(1:37-1:38),Int(1:39-1:40),CloseRound(1:40-1:41),NoSpaceOpenRound(1:41-1:42),Int(1:42-1:43),CloseRound(1:43-1:44),EndOfFile(1:44-1:44),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.44
	(e-apply @1.1-1.41
		(e-tuple @1.1-1.30
			(e-lambda @1.2-1.29
				(args
					(p-ident @1.3-1.4 (raw "a"))
					(p-ident @1.6-1.7 (raw "b"))
					(p-ident @1.9-1.10 (raw "c")))
				(e-lambda @1.12-1.29
					(args
						(p-ident @1.13-1.14 (raw "x")))
					(e-binop @1.16-1.29 (op "+")
						(e-ident @1.16-1.17 (raw "a"))
						(e-binop @1.20-1.29 (op "+")
							(e-ident @1.20-1.21 (raw "b"))
							(e-binop @1.24-1.29 (op "+")
								(e-ident @1.24-1.25 (raw "c"))
								(e-ident @1.28-1.29 (raw "x"))))))))
		(e-int @1.31-1.33 (raw "10"))
		(e-int @1.35-1.37 (raw "20"))
		(e-int @1.39-1.40 (raw "5")))
	(e-int @1.42-1.43 (raw "7")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.44
	(e-call @1.1-1.41
		(e-closure @1.2-1.29
			(e-lambda @1.2-1.29
				(args
					(p-assign @1.3-1.4 (ident "a"))
					(p-assign @1.6-1.7 (ident "b"))
					(p-assign @1.9-1.10 (ident "c")))
				(e-closure @1.12-1.29
					(captures
						(capture @1.9-1.10 (ident "c"))
						(capture @1.3-1.4 (ident "a"))
						(capture @1.6-1.7 (ident "b")))
					(e-lambda @1.12-1.29
						(args
							(p-assign @1.13-1.14 (ident "x")))
						(e-binop @1.16-1.29 (op "add")
							(e-lookup-local @1.16-1.17
								(p-assign @1.3-1.4 (ident "a")))
							(e-binop @1.20-1.29 (op "add")
								(e-lookup-local @1.20-1.21
									(p-assign @1.6-1.7 (ident "b")))
								(e-binop @1.24-1.29 (op "add")
									(e-lookup-local @1.24-1.25
										(p-assign @1.9-1.10 (ident "c")))
									(e-lookup-local @1.28-1.29
										(p-assign @1.13-1.14 (ident "x"))))))))))
		(e-int @1.31-1.33 (value "10"))
		(e-int @1.35-1.37 (value "20"))
		(e-int @1.39-1.40 (value "5")))
	(e-int @1.42-1.43 (value "7")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.44 (type "Num(_size)"))
~~~
