# META
~~~ini
description=Deep nesting with multiple captures - five-level nested lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|a| |b| |c| |d| |e| a + b + c + d + e)(1)(2)(3)(4)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),LowerIdent(1:3-1:4),OpBar(1:4-1:5),OpBar(1:6-1:7),LowerIdent(1:7-1:8),OpBar(1:8-1:9),OpBar(1:10-1:11),LowerIdent(1:11-1:12),OpBar(1:12-1:13),OpBar(1:14-1:15),LowerIdent(1:15-1:16),OpBar(1:16-1:17),OpBar(1:18-1:19),LowerIdent(1:19-1:20),OpBar(1:20-1:21),LowerIdent(1:22-1:23),OpPlus(1:24-1:25),LowerIdent(1:26-1:27),OpPlus(1:28-1:29),LowerIdent(1:30-1:31),OpPlus(1:32-1:33),LowerIdent(1:34-1:35),OpPlus(1:36-1:37),LowerIdent(1:38-1:39),CloseRound(1:39-1:40),NoSpaceOpenRound(1:40-1:41),Int(1:41-1:42),CloseRound(1:42-1:43),NoSpaceOpenRound(1:43-1:44),Int(1:44-1:45),CloseRound(1:45-1:46),NoSpaceOpenRound(1:46-1:47),Int(1:47-1:48),CloseRound(1:48-1:49),NoSpaceOpenRound(1:49-1:50),Int(1:50-1:51),CloseRound(1:51-1:52),NoSpaceOpenRound(1:52-1:53),Int(1:53-1:54),CloseRound(1:54-1:55),EndOfFile(1:55-1:55),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.55
	(e-apply @1.1-1.52
		(e-apply @1.1-1.49
			(e-apply @1.1-1.46
				(e-apply @1.1-1.43
					(e-tuple @1.1-1.40
						(e-lambda @1.2-1.39
							(args
								(p-ident @1.3-1.4 (raw "a")))
							(e-lambda @1.6-1.39
								(args
									(p-ident @1.7-1.8 (raw "b")))
								(e-lambda @1.10-1.39
									(args
										(p-ident @1.11-1.12 (raw "c")))
									(e-lambda @1.14-1.39
										(args
											(p-ident @1.15-1.16 (raw "d")))
										(e-lambda @1.18-1.39
											(args
												(p-ident @1.19-1.20 (raw "e")))
											(e-binop @1.22-1.39 (op "+")
												(e-ident @1.22-1.23 (raw "a"))
												(e-binop @1.26-1.39 (op "+")
													(e-ident @1.26-1.27 (raw "b"))
													(e-binop @1.30-1.39 (op "+")
														(e-ident @1.30-1.31 (raw "c"))
														(e-binop @1.34-1.39 (op "+")
															(e-ident @1.34-1.35 (raw "d"))
															(e-ident @1.38-1.39 (raw "e"))))))))))))
					(e-int @1.41-1.42 (raw "1")))
				(e-int @1.44-1.45 (raw "2")))
			(e-int @1.47-1.48 (raw "3")))
		(e-int @1.50-1.51 (raw "4")))
	(e-int @1.53-1.54 (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.55
	(e-call @1.1-1.52
		(e-call @1.1-1.49
			(e-call @1.1-1.46
				(e-call @1.1-1.43
					(e-lambda @1.2-1.39
						(args
							(p-assign @1.3-1.4 (ident "a")))
						(e-lambda @1.6-1.39
							(args
								(p-assign @1.7-1.8 (ident "b")))
							(e-lambda @1.10-1.39
								(args
									(p-assign @1.11-1.12 (ident "c")))
								(e-lambda @1.14-1.39
									(args
										(p-assign @1.15-1.16 (ident "d")))
									(e-lambda @1.18-1.39
										(args
											(p-assign @1.19-1.20 (ident "e")))
										(captures
											(capture @1.3-1.4 (ident "a"))
											(capture @1.7-1.8 (ident "b"))
											(capture @1.11-1.12 (ident "c"))
											(capture @1.15-1.16 (ident "d")))
										(e-binop @1.22-1.39 (op "add")
											(e-lookup-local @1.22-1.23
												(p-assign @1.3-1.4 (ident "a")))
											(e-binop @1.26-1.39 (op "add")
												(e-lookup-local @1.26-1.27
													(p-assign @1.7-1.8 (ident "b")))
												(e-binop @1.30-1.39 (op "add")
													(e-lookup-local @1.30-1.31
														(p-assign @1.11-1.12 (ident "c")))
													(e-binop @1.34-1.39 (op "add")
														(e-lookup-local @1.34-1.35
															(p-assign @1.15-1.16 (ident "d")))
														(e-lookup-local @1.38-1.39
															(p-assign @1.19-1.20 (ident "e"))))))))))))
					(e-int @1.41-1.42 (value "1")))
				(e-int @1.44-1.45 (value "2")))
			(e-int @1.47-1.48 (value "3")))
		(e-int @1.50-1.51 (value "4")))
	(e-int @1.53-1.54 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.55 (type "Num(_size)"))
~~~
