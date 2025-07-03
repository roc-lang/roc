# META
~~~ini
description=Multiple uses of same type variable in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

pair : a -> (a, a)
pair = |x| (x, x)

main! = |_| {}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpColon(3:6-3:7),LowerIdent(3:8-3:9),OpArrow(3:10-3:12),OpenRound(3:13-3:14),LowerIdent(3:14-3:15),Comma(3:15-3:16),LowerIdent(3:17-3:18),CloseRound(3:18-3:19),Newline(1:1-1:1),
LowerIdent(4:1-4:5),OpAssign(4:6-4:7),OpBar(4:8-4:9),LowerIdent(4:9-4:10),OpBar(4:10-4:11),OpenRound(4:12-4:13),LowerIdent(4:13-4:14),Comma(4:14-4:15),LowerIdent(4:16-4:17),CloseRound(4:17-4:18),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file @1.1-6.15
	(app @1.1-1.53
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.53 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.53 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3.1-4.5 (name "pair")
			(ty-fn @3.8-3.19
				(ty-var @3.8-3.9 (raw "a"))
				(ty-tuple @3.13-3.19
					(ty-var @3.14-3.15 (raw "a"))
					(ty-var @3.17-3.18 (raw "a")))))
		(s-decl @4.1-4.18
			(p-ident @4.1-4.5 (raw "pair"))
			(e-lambda @4.8-4.18
				(args
					(p-ident @4.9-4.10 (raw "x")))
				(e-tuple @4.12-4.18
					(e-ident @4.13-4.14 (qaul "") (raw "x"))
					(e-ident @4.16-4.17 (qaul "") (raw "x")))))
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
		(p-assign @4.1-4.5 (ident "pair"))
		(e-lambda @4.8-4.18
			(args
				(p-assign @4.9-4.10 (ident "x")))
			(e-tuple @4.12-4.18
				(elems
					(e-lookup-local @4.13-4.14
						(pattern @4.9-4.10))
					(e-lookup-local @4.16-4.17
						(pattern @4.9-4.10)))))
		(annotation @4.1-4.5
			(declared-type
				(ty-fn @3.8-3.19 (effectful false)
					(ty-var @3.8-3.9 (name "a"))
					(ty-tuple @3.13-3.19
						(ty-var @3.14-3.15 (name "a"))
						(ty-var @3.17-3.18 (name "a")))))))
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
		(patt @4.1-4.5 (type "a -> (a, a)"))
		(patt @6.1-6.6 (type "a -> {}")))
	(expressions
		(expr @4.8-4.18 (type "a -> (a, a)"))
		(expr @6.9-6.15 (type "a -> {}"))))
~~~
