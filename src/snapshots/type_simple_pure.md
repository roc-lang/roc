# META
~~~ini
description=Simple pure function type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : Str -> Str
identity = |x| x

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),UpperIdent(3:12-3:15),OpArrow(3:16-3:18),UpperIdent(3:19-3:22),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:14),OpBar(4:14-4:15),LowerIdent(4:16-4:17),
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
		(s-type-anno @3.1-3.22 (name "identity")
			(ty-fn @3.12-3.22
				(ty @3.12-3.15 (name "Str"))
				(ty @3.19-3.22 (name "Str"))))
		(s-decl @4.1-4.17
			(p-ident @4.1-4.9 (raw "identity"))
			(e-lambda @4.12-4.17
				(args
					(p-ident @4.13-4.14 (raw "x")))
				(e-ident @4.16-4.17 (raw "x"))))
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
		(p-assign @4.1-4.9 (ident "identity"))
		(e-lambda @4.12-4.17
			(args
				(p-assign @4.13-4.14 (ident "x")))
			(e-lookup-local @4.16-4.17
				(p-assign @4.13-4.14 (ident "x"))))
		(annotation @4.1-4.9
			(declared-type
				(ty-fn @3.12-3.22 (effectful false)
					(ty @3.12-3.15 (name "Str"))
					(ty @3.19-3.22 (name "Str"))))))
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
		(patt @4.1-4.9 (type "str -> str"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.12-4.17 (type "str -> str"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
