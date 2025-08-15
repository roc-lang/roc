# META
~~~ini
description=Record as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x, y }| x * y)({ x: 10, y: 20 })
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:5-1:6),Comma(1:6-1:7),LowerIdent(1:8-1:9),CloseCurly(1:10-1:11),OpBar(1:11-1:12),LowerIdent(1:13-1:14),OpStar(1:15-1:16),LowerIdent(1:17-1:18),CloseRound(1:18-1:19),NoSpaceOpenRound(1:19-1:20),OpenCurly(1:20-1:21),LowerIdent(1:22-1:23),OpColon(1:23-1:24),Int(1:25-1:27),Comma(1:27-1:28),LowerIdent(1:29-1:30),OpColon(1:30-1:31),Int(1:32-1:34),CloseCurly(1:35-1:36),CloseRound(1:36-1:37),EndOfFile(1:37-1:37),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.37
	(e-tuple @1.1-1.19
		(e-lambda @1.2-1.18
			(args
				(p-record @1.3-1.11
					(field @1.5-1.6 (name "x") (rest false))
					(field @1.8-1.9 (name "y") (rest false))))
			(e-binop @1.13-1.18 (op "*")
				(e-ident @1.13-1.14 (raw "x"))
				(e-ident @1.17-1.18 (raw "y")))))
	(e-record @1.20-1.36
		(field (field "x")
			(e-int @1.25-1.27 (raw "10")))
		(field (field "y")
			(e-int @1.32-1.34 (raw "20")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.37
	(e-lambda @1.2-1.18
		(args
			(p-record-destructure @1.3-1.11
				(destructs
					(record-destruct @1.5-1.6 (label "x") (ident "x")
						(required
							(p-assign @1.5-1.6 (ident "x"))))
					(record-destruct @1.8-1.9 (label "y") (ident "y")
						(required
							(p-assign @1.8-1.9 (ident "y")))))))
		(e-binop @1.13-1.18 (op "mul")
			(e-lookup-local @1.13-1.14
				(p-assign @1.5-1.6 (ident "x")))
			(e-lookup-local @1.17-1.18
				(p-assign @1.8-1.9 (ident "y")))))
	(e-record @1.20-1.36
		(fields
			(field (name "x")
				(e-int @1.25-1.27 (value "10")))
			(field (name "y")
				(e-int @1.32-1.34 (value "20"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.37 (type "_a"))
~~~
