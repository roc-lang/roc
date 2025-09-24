# META
~~~ini
description=Record with single field as an argument
type=expr
~~~
# SOURCE
~~~roc
(|{ x }| x )({ x: -10 })
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpBar(1:2-1:3),OpenCurly(1:3-1:4),LowerIdent(1:5-1:6),CloseCurly(1:7-1:8),OpBar(1:8-1:9),LowerIdent(1:10-1:11),CloseRound(1:12-1:13),NoSpaceOpenRound(1:13-1:14),OpenCurly(1:14-1:15),LowerIdent(1:16-1:17),OpColon(1:17-1:18),Int(1:19-1:22),CloseCurly(1:23-1:24),CloseRound(1:24-1:25),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.25
	(e-tuple @1.1-1.13
		(e-lambda @1.2-1.11
			(args
				(p-record @1.3-1.8
					(field @1.5-1.6 (name "x") (rest false))))
			(e-ident @1.10-1.11 (raw "x"))))
	(e-record @1.14-1.24
		(field (field "x")
			(e-int @1.19-1.22 (raw "-10")))))
~~~
# FORMATTED
~~~roc
(|{ x }| x)({ x: -10 })
~~~
# CANONICALIZE
~~~clojure
(e-call @1.1-1.25
	(e-lambda @1.2-1.11
		(args
			(p-record-destructure @1.3-1.8
				(destructs
					(record-destruct @1.5-1.6 (label "x") (ident "x")
						(required
							(p-assign @1.5-1.6 (ident "x")))))))
		(e-lookup-local @1.10-1.11
			(p-assign @1.5-1.6 (ident "x"))))
	(e-record @1.14-1.24
		(fields
			(field (name "x")
				(e-num @1.19-1.22 (value "-10"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.25 (type "_a"))
~~~
