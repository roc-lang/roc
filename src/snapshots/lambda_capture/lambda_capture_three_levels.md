# META
~~~ini
description=Three-level nested lambda captures - innermost lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|outer| |middle| |inner| outer + middle + inner)(1)(2)(3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:8),OpBar(1:8-1:9),OpBar(1:10-1:11),LowerIdent(1:11-1:17),OpBar(1:17-1:18),OpBar(1:19-1:20),LowerIdent(1:20-1:25),OpBar(1:25-1:26),LowerIdent(1:27-1:32),OpPlus(1:33-1:34),LowerIdent(1:35-1:41),OpPlus(1:42-1:43),LowerIdent(1:44-1:49),CloseRound(1:49-1:50),NoSpaceOpenRound(1:50-1:51),Int(1:51-1:52),CloseRound(1:52-1:53),NoSpaceOpenRound(1:53-1:54),Int(1:54-1:55),CloseRound(1:55-1:56),NoSpaceOpenRound(1:56-1:57),Int(1:57-1:58),CloseRound(1:58-1:59),EndOfFile(1:59-1:59),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.59
	(e-apply @1.1-1.56
		(e-apply @1.1-1.53
			(e-tuple @1.1-1.50
				(e-lambda @1.2-1.49
					(args
						(p-ident @1.3-1.8 (raw "outer")))
					(e-lambda @1.10-1.49
						(args
							(p-ident @1.11-1.17 (raw "middle")))
						(e-lambda @1.19-1.49
							(args
								(p-ident @1.20-1.25 (raw "inner")))
							(e-binop @1.27-1.49 (op "+")
								(e-ident @1.27-1.32 (raw "outer"))
								(e-binop @1.35-1.49 (op "+")
									(e-ident @1.35-1.41 (raw "middle"))
									(e-ident @1.44-1.49 (raw "inner"))))))))
			(e-int @1.51-1.52 (raw "1")))
		(e-int @1.54-1.55 (raw "2")))
	(e-int @1.57-1.58 (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.59
	(e-call @1.1-1.56
		(e-call @1.1-1.53
			(e-lambda @1.2-1.49
				(args
					(p-assign @1.3-1.8 (ident "outer")))
				(e-lambda @1.10-1.49
					(args
						(p-assign @1.11-1.17 (ident "middle")))
					(e-lambda @1.19-1.49
						(args
							(p-assign @1.20-1.25 (ident "inner")))
						(captures
							(capture (name "outer"))
							(capture (name "middle")))
						(e-binop @1.27-1.49 (op "add")
							(e-lookup-local @1.27-1.32
								(p-assign @1.3-1.8 (ident "outer")))
							(e-binop @1.35-1.49 (op "add")
								(e-lookup-local @1.35-1.41
									(p-assign @1.11-1.17 (ident "middle")))
								(e-lookup-local @1.44-1.49
									(p-assign @1.20-1.25 (ident "inner"))))))))
			(e-int @1.51-1.52 (value "1")))
		(e-int @1.54-1.55 (value "2")))
	(e-int @1.57-1.58 (value "3")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.59 (type "Num(_size)"))
~~~
