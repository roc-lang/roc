# META
~~~ini
description=Basic type variable introduction in type annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Type variable 'a' introduced in annotation and used in body
identity : a -> a
identity = |a| a

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:62),
LowerIdent(4:1-4:9),OpColon(4:10-4:11),LowerIdent(4:12-4:13),OpArrow(4:14-4:16),LowerIdent(4:17-4:18),Newline(1:1-1:1),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpBar(5:12-5:13),LowerIdent(5:13-5:14),OpBar(5:14-5:15),LowerIdent(5:16-5:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),OpBar(7:9-7:10),Underscore(7:10-7:11),OpBar(7:11-7:12),OpenCurly(7:13-7:14),CloseCurly(7:14-7:15),EndOfFile(7:15-7:15),
~~~
# PARSE
~~~clojure
(file @1.1-7.15
	(app @1.1-1.57
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.57 (name "pf") (optional false)
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.57 (name "pf") (optional false)
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-5.9 (name "identity")
			(ty-fn @4.12-4.18
				(ty-var @4.12-4.13 (raw "a"))
				(ty-var @4.17-4.18 (raw "a"))))
		(s-decl @5.1-5.17
			(p-ident @5.1-5.9 (raw "identity"))
			(e-lambda @5.12-5.17
				(args
					(p-ident @5.13-5.14 (raw "a")))
				(e-ident @5.16-5.17 (raw "a"))))
		(s-decl @7.1-7.15
			(p-ident @7.1-7.6 (raw "main!"))
			(e-lambda @7.9-7.15
				(args
					(p-underscore))
				(e-record @7.13-7.15)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.9 (ident "identity"))
		(e-lambda @5.12-5.17
			(args
				(p-assign @5.13-5.14 (ident "a")))
			(e-lookup-local @5.16-5.17
				(p-assign @5.13-5.14 (ident "a"))))
		(annotation @5.1-5.9
			(declared-type
				(ty-fn @4.12-4.18 (effectful false)
					(ty-var @4.12-4.13 (name "a"))
					(ty-var @4.17-4.18 (name "a"))))))
	(d-let
		(p-assign @7.1-7.6 (ident "main!"))
		(e-lambda @7.9-7.15
			(args
				(p-underscore @7.10-7.11))
			(e-empty_record @7.13-7.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.9 (type "a -> a"))
		(patt @7.1-7.6 (type "* -> {}")))
	(expressions
		(expr @5.12-5.17 (type "a -> a"))
		(expr @7.9-7.15 (type "* -> {}"))))
~~~
