# META
~~~ini
description=Mixed destructue patterns
type=expr
~~~
# SOURCE
~~~roc
(|{ a, x: (b, c), y: { d, e }}| a + b + c + d + e )({ a: 1, x: (2, 3), y: {d: 4, e: 5}})
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:5-1:6),Comma(1:6-1:7),LowerIdent(1:8-1:9),OpColon(1:9-1:10),OpenRound(1:11-1:12),LowerIdent(1:12-1:13),Comma(1:13-1:14),LowerIdent(1:15-1:16),CloseRound(1:16-1:17),Comma(1:17-1:18),LowerIdent(1:19-1:20),OpColon(1:20-1:21),OpenCurly(1:22-1:23),LowerIdent(1:24-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:28),CloseCurly(1:29-1:30),CloseCurly(1:30-1:31),OpBar(1:31-1:32),LowerIdent(1:33-1:34),OpPlus(1:35-1:36),LowerIdent(1:37-1:38),OpPlus(1:39-1:40),LowerIdent(1:41-1:42),OpPlus(1:43-1:44),LowerIdent(1:45-1:46),OpPlus(1:47-1:48),LowerIdent(1:49-1:50),CloseRound(1:51-1:52),NoSpaceOpenRound(1:52-1:53),OpenCurly(1:53-1:54),LowerIdent(1:55-1:56),OpColon(1:56-1:57),Int(1:58-1:59),Comma(1:59-1:60),LowerIdent(1:61-1:62),OpColon(1:62-1:63),OpenRound(1:64-1:65),Int(1:65-1:66),Comma(1:66-1:67),Int(1:68-1:69),CloseRound(1:69-1:70),Comma(1:70-1:71),LowerIdent(1:72-1:73),OpColon(1:73-1:74),OpenCurly(1:75-1:76),LowerIdent(1:76-1:77),OpColon(1:77-1:78),Int(1:79-1:80),Comma(1:80-1:81),LowerIdent(1:82-1:83),OpColon(1:83-1:84),Int(1:85-1:86),CloseCurly(1:86-1:87),CloseCurly(1:87-1:88),CloseRound(1:88-1:89),EndOfFile(1:89-1:89),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.89
	(e-tuple @1.1-1.52
		(e-lambda @1.2-1.50
			(args
				(p-record @1.3-1.31
					(field @1.5-1.6 (name "a") (rest false))
					(field @1.8-1.17 (name "x") (rest false)
						(p-tuple @1.11-1.17
							(p-ident @1.12-1.13 (raw "b"))
							(p-ident @1.15-1.16 (raw "c"))))
					(field @1.19-1.30 (name "y") (rest false)
						(p-record @1.22-1.30
							(field @1.24-1.25 (name "d") (rest false))
							(field @1.27-1.28 (name "e") (rest false))))))
			(e-binop @1.33-1.50 (op "+")
				(e-binop @1.33-1.46 (op "+")
					(e-binop @1.33-1.42 (op "+")
						(e-binop @1.33-1.38 (op "+")
							(e-ident @1.33-1.34 (raw "a"))
							(e-ident @1.37-1.38 (raw "b")))
						(e-ident @1.41-1.42 (raw "c")))
					(e-ident @1.45-1.46 (raw "d")))
				(e-ident @1.49-1.50 (raw "e")))))
	(e-record @1.53-1.88
		(field (field "a")
			(e-int @1.58-1.59 (raw "1")))
		(field (field "x")
			(e-tuple @1.64-1.70
				(e-int @1.65-1.66 (raw "2"))
				(e-int @1.68-1.69 (raw "3"))))
		(field (field "y")
			(e-record @1.75-1.87
				(field (field "d")
					(e-int @1.79-1.80 (raw "4")))
				(field (field "e")
					(e-int @1.85-1.86 (raw "5")))))))
~~~
# FORMATTED
~~~roc
(|{ a, x: (b, c), y: { d, e } }| a + b + c + d + e)({ a: 1, x: (2, 3), y: { d: 4, e: 5 } })
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.89
	(e-lambda @1.2-1.50
		(args
			(p-record-destructure @1.3-1.31
				(destructs
					(record-destruct @1.5-1.6 (label "a") (ident "a")
						(required
							(p-assign @1.5-1.6 (ident "a"))))
					(record-destruct @1.8-1.17 (label "x") (ident "x")
						(sub-pattern
							(p-tuple @1.11-1.17
								(patterns
									(p-assign @1.12-1.13 (ident "b"))
									(p-assign @1.15-1.16 (ident "c"))))))
					(record-destruct @1.19-1.30 (label "y") (ident "y")
						(sub-pattern
							(p-record-destructure @1.22-1.30
								(destructs
									(record-destruct @1.24-1.25 (label "d") (ident "d")
										(required
											(p-assign @1.24-1.25 (ident "d"))))
									(record-destruct @1.27-1.28 (label "e") (ident "e")
										(required
											(p-assign @1.27-1.28 (ident "e")))))))))))
		(e-binop @1.33-1.50 (op "add")
			(e-binop @1.33-1.46 (op "add")
				(e-binop @1.33-1.42 (op "add")
					(e-binop @1.33-1.38 (op "add")
						(e-lookup-local @1.33-1.34
							(p-assign @1.5-1.6 (ident "a")))
						(e-lookup-local @1.37-1.38
							(p-assign @1.12-1.13 (ident "b"))))
					(e-lookup-local @1.41-1.42
						(p-assign @1.15-1.16 (ident "c"))))
				(e-lookup-local @1.45-1.46
					(p-assign @1.24-1.25 (ident "d"))))
			(e-lookup-local @1.49-1.50
				(p-assign @1.27-1.28 (ident "e")))))
	(e-record @1.53-1.88
		(fields
			(field (name "a")
				(e-int @1.58-1.59 (value "1")))
			(field (name "x")
				(e-tuple @1.64-1.70
					(elems
						(e-int @1.65-1.66 (value "2"))
						(e-int @1.68-1.69 (value "3")))))
			(field (name "y")
				(e-record @1.75-1.87
					(fields
						(field (name "d")
							(e-int @1.79-1.80 (value "4")))
						(field (name "e")
							(e-int @1.85-1.86 (value "5")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.89 (type "_f"))
~~~
