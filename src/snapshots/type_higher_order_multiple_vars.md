# META
~~~ini
description=Higher-order function with multiple type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
compose = |f| |g| |x| f(g(x))

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenRound(3:11-3:12),NamedUnderscore(3:12-3:14),OpArrow(3:15-3:17),NamedUnderscore(3:18-3:20),CloseRound(3:20-3:21),OpArrow(3:22-3:24),OpenRound(3:25-3:26),NamedUnderscore(3:26-3:28),OpArrow(3:29-3:31),NamedUnderscore(3:32-3:34),CloseRound(3:34-3:35),OpArrow(3:36-3:38),OpenRound(3:39-3:40),NamedUnderscore(3:40-3:42),OpArrow(3:43-3:45),NamedUnderscore(3:46-3:48),CloseRound(3:48-3:49),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:13),OpBar(4:13-4:14),OpBar(4:15-4:16),LowerIdent(4:16-4:17),OpBar(4:17-4:18),OpBar(4:19-4:20),LowerIdent(4:20-4:21),OpBar(4:21-4:22),LowerIdent(4:23-4:24),NoSpaceOpenRound(4:24-4:25),LowerIdent(4:25-4:26),NoSpaceOpenRound(4:26-4:27),LowerIdent(4:27-4:28),CloseRound(4:28-4:29),CloseRound(4:29-4:30),
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
		(s-type-anno @3.1-3.49 (name "compose")
			(ty-fn @3.11-3.49
				(ty-fn @3.12-3.20
					(underscore-ty-var @3.12-3.14 (raw "_b"))
					(underscore-ty-var @3.18-3.20 (raw "_c")))
				(ty-fn @3.25-3.49
					(ty-fn @3.26-3.34
						(underscore-ty-var @3.26-3.28 (raw "_a"))
						(underscore-ty-var @3.32-3.34 (raw "_b")))
					(ty-fn @3.40-3.48
						(underscore-ty-var @3.40-3.42 (raw "_a"))
						(underscore-ty-var @3.46-3.48 (raw "_c"))))))
		(s-decl @4.1-4.30
			(p-ident @4.1-4.8 (raw "compose"))
			(e-lambda @4.11-4.30
				(args
					(p-ident @4.12-4.13 (raw "f")))
				(e-lambda @4.15-4.30
					(args
						(p-ident @4.16-4.17 (raw "g")))
					(e-lambda @4.19-4.30
						(args
							(p-ident @4.20-4.21 (raw "x")))
						(e-apply @4.23-4.30
							(e-ident @4.23-4.24 (raw "f"))
							(e-apply @4.25-4.29
								(e-ident @4.25-4.26 (raw "g"))
								(e-ident @4.27-4.28 (raw "x"))))))))
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
		(p-assign @4.1-4.8 (ident "compose"))
		(e-lambda @4.11-4.30
			(args
				(p-assign @4.12-4.13 (ident "f")))
			(e-lambda @4.15-4.30
				(args
					(p-assign @4.16-4.17 (ident "g")))
				(e-lambda @4.19-4.30
					(args
						(p-assign @4.20-4.21 (ident "x")))
					(e-call @4.23-4.30
						(e-lookup-local @4.23-4.24
							(p-assign @4.12-4.13 (ident "f")))
						(e-call @4.25-4.29
							(e-lookup-local @4.25-4.26
								(p-assign @4.16-4.17 (ident "g")))
							(e-lookup-local @4.27-4.28
								(p-assign @4.20-4.21 (ident "x"))))))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.49 (effectful false)
					(ty-parens @3.11-3.21
						(ty-fn @3.12-3.20 (effectful false)
							(ty-var @3.12-3.14 (name "_b"))
							(ty-var @3.18-3.20 (name "_c"))))
					(ty-fn @3.25-3.49 (effectful false)
						(ty-parens @3.25-3.35
							(ty-fn @3.26-3.34 (effectful false)
								(ty-var @3.26-3.28 (name "_a"))
								(ty-var @3.32-3.34 (name "_b"))))
						(ty-parens @3.39-3.49
							(ty-fn @3.40-3.48 (effectful false)
								(ty-var @3.40-3.42 (name "_a"))
								(ty-var @3.46-3.48 (name "_c")))))))))
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
		(patt @4.1-4.8 (type "_b -> _c -> _a -> _b -> _a -> _c"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.11-4.30 (type "_b -> _c -> _a -> _b -> _a -> _c"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
