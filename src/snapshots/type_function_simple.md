# META
~~~ini
description=Simple function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

apply : (_a -> _b) -> _a -> _b
apply = |fn| |x| fn(x)

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),OpenRound(3:9-3:10),NamedUnderscore(3:10-3:12),OpArrow(3:13-3:15),NamedUnderscore(3:16-3:18),CloseRound(3:18-3:19),OpArrow(3:20-3:22),NamedUnderscore(3:23-3:25),OpArrow(3:26-3:28),NamedUnderscore(3:29-3:31),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:12),OpBar(4:12-4:13),OpBar(4:14-4:15),LowerIdent(4:15-4:16),OpBar(4:16-4:17),LowerIdent(4:18-4:20),NoSpaceOpenRound(4:20-4:21),LowerIdent(4:21-4:22),CloseRound(4:22-4:23),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
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
		(s-type-anno @3.1-3.31 (name "apply")
			(ty-fn @3.9-3.31
				(ty-fn @3.10-3.18
					(underscore-ty-var @3.10-3.12 (raw "_a"))
					(underscore-ty-var @3.16-3.18 (raw "_b")))
				(ty-fn @3.23-3.31
					(underscore-ty-var @3.23-3.25 (raw "_a"))
					(underscore-ty-var @3.29-3.31 (raw "_b")))))
		(s-decl @4.1-4.23
			(p-ident @4.1-4.6 (raw "apply"))
			(e-lambda @4.9-4.23
				(args
					(p-ident @4.10-4.12 (raw "fn")))
				(e-lambda @4.14-4.23
					(args
						(p-ident @4.15-4.16 (raw "x")))
					(e-apply @4.18-4.23
						(e-ident @4.18-4.20 (raw "fn"))
						(e-ident @4.21-4.22 (raw "x"))))))
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
		(p-assign @4.1-4.6 (ident "apply"))
		(e-lambda @4.9-4.23
			(args
				(p-assign @4.10-4.12 (ident "fn")))
			(e-lambda @4.14-4.23
				(args
					(p-assign @4.15-4.16 (ident "x")))
				(e-call @4.18-4.23
					(e-lookup-local @4.18-4.20
						(p-assign @4.10-4.12 (ident "fn")))
					(e-lookup-local @4.21-4.22
						(p-assign @4.15-4.16 (ident "x"))))))
		(annotation @4.1-4.6
			(declared-type
				(ty-fn @3.9-3.31 (effectful false)
					(ty-parens @3.9-3.19
						(ty-fn @3.10-3.18 (effectful false)
							(ty-var @3.10-3.12 (name "_a"))
							(ty-var @3.16-3.18 (name "_b"))))
					(ty-fn @3.23-3.31 (effectful false)
						(ty-var @3.23-3.25 (name "_a"))
						(ty-var @3.29-3.31 (name "_b")))))))
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
		(patt @4.1-4.6 (type "_a -> _b -> _a -> _b"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.9-4.23 (type "_a -> _b -> _a -> _b"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
