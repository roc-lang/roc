# META
~~~ini
description=Two distinct type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),OpenRound(3:8-3:9),LowerIdent(3:9-3:10),Comma(3:10-3:11),LowerIdent(3:12-3:13),CloseRound(3:13-3:14),OpArrow(3:15-3:17),OpenRound(3:18-3:19),LowerIdent(3:19-3:20),Comma(3:20-3:21),LowerIdent(3:22-3:23),CloseRound(3:23-3:24),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),NoSpaceOpenRound(4:9-4:10),LowerIdent(4:10-4:11),Comma(4:11-4:12),LowerIdent(4:13-4:14),CloseRound(4:14-4:15),OpBar(4:15-4:16),OpenRound(4:17-4:18),LowerIdent(4:18-4:19),Comma(4:19-4:20),LowerIdent(4:21-4:22),CloseRound(4:22-4:23),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.15
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-3.24 (name "swap")
			(ty-fn @3.8-3.24
				(ty-tuple @3.8-3.14
					(ty-var @3.9-3.10 (raw "a"))
					(ty-var @3.12-3.13 (raw "b")))
				(ty-tuple @3.18-3.24
					(ty-var @3.19-3.20 (raw "b"))
					(ty-var @3.22-3.23 (raw "a")))))
		(s-decl @4.1-4.23
			(p-ident @4.1-4.5 (raw "swap"))
			(e-lambda @4.8-4.23
				(args
					(p-tuple @4.9-4.15
						(p-ident @4.10-4.11 (raw "x"))
						(p-ident @4.13-4.14 (raw "y"))))
				(e-tuple @4.17-4.23
					(e-ident @4.18-4.19 (raw "y"))
					(e-ident @4.21-4.22 (raw "x")))))
		(s-decl @6.1-6.15
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.15
				(args
					(p-underscore))
				(e-record @6.13-6.15)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.5 (ident "swap"))
		(e-lambda @4.8-4.23
			(args
				(p-tuple @4.9-4.15
					(patterns
						(p-assign @4.10-4.11 (ident "x"))
						(p-assign @4.13-4.14 (ident "y")))))
			(e-tuple @4.17-4.23
				(elems
					(e-lookup-local @4.18-4.19
						(p-assign @4.13-4.14 (ident "y")))
					(e-lookup-local @4.21-4.22
						(p-assign @4.10-4.11 (ident "x"))))))
		(annotation @4.1-4.5
			(declared-type
				(ty-fn @3.8-3.24 (effectful false)
					(ty-tuple @3.8-3.14
						(ty-var @3.9-3.10 (name "a"))
						(ty-var @3.12-3.13 (name "b")))
					(ty-tuple @3.18-3.24
						(ty-var @3.19-3.20 (name "b"))
						(ty-var @3.22-3.23 (name "a")))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-lambda @6.9-6.15
			(args
				(p-underscore @6.10-6.11))
			(e-empty_record @6.13-6.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.5 (type "(a, b) -> (b, a)"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.8-4.23 (type "(a, b) -> (b, a)"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
