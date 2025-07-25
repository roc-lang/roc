# META
~~~ini
description=Type variable connection between function annotation and body
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| {
    thing : a  # refers to the type var introduced in function type annotation
    thing = x  # refers to the value from the function parameter
    thing
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),LowerIdent(3:12-3:13),OpArrow(3:14-3:16),LowerIdent(3:17-3:18),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:14),OpBar(4:14-4:15),OpenCurly(4:16-4:17),
LowerIdent(5:5-5:10),OpColon(5:11-5:12),LowerIdent(5:13-5:14),
LowerIdent(6:5-6:10),OpAssign(6:11-6:12),LowerIdent(6:13-6:14),
LowerIdent(7:5-7:10),
CloseCurly(8:1-8:2),
LowerIdent(10:1-10:6),OpAssign(10:7-10:8),OpBar(10:9-10:10),Underscore(10:10-10:11),OpBar(10:11-10:12),OpenCurly(10:13-10:14),CloseCurly(10:14-10:15),EndOfFile(10:15-10:15),
~~~
# PARSE
~~~clojure
(file @1.1-10.15
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
		(s-type-anno @3.1-3.18 (name "identity")
			(ty-fn @3.12-3.18
				(ty-var @3.12-3.13 (raw "a"))
				(ty-var @3.17-3.18 (raw "a"))))
		(s-decl @4.1-8.2
			(p-ident @4.1-4.9 (raw "identity"))
			(e-lambda @4.12-8.2
				(args
					(p-ident @4.13-4.14 (raw "x")))
				(e-block @4.16-8.2
					(statements
						(s-type-anno @5.5-5.14 (name "thing")
							(ty-var @5.13-5.14 (raw "a")))
						(s-decl @6.5-6.14
							(p-ident @6.5-6.10 (raw "thing"))
							(e-ident @6.13-6.14 (raw "x")))
						(e-ident @7.5-7.10 (raw "thing"))))))
		(s-decl @10.1-10.15
			(p-ident @10.1-10.6 (raw "main!"))
			(e-lambda @10.9-10.15
				(args
					(p-underscore))
				(e-record @10.13-10.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

identity : a -> a
identity = |x| {
	thing : a # refers to the type var introduced in function type annotation
	thing = x # refers to the value from the function parameter
	thing
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.9 (ident "identity"))
		(e-closure @4.12-8.2
			(e-lambda @4.12-8.2
				(args
					(p-assign @4.13-4.14 (ident "x")))
				(e-block @4.16-8.2
					(s-type-anno @5.5-5.14 (name "thing")
						(ty-var @5.13-5.14 (name "a")))
					(s-let @6.5-6.14
						(p-assign @6.5-6.10 (ident "thing"))
						(e-lookup-local @6.13-6.14
							(p-assign @4.13-4.14 (ident "x"))))
					(e-lookup-local @7.5-7.10
						(p-assign @6.5-6.10 (ident "thing"))))))
		(annotation @4.1-4.9
			(declared-type
				(ty-fn @3.12-3.18 (effectful false)
					(ty-var @3.12-3.13 (name "a"))
					(ty-var @3.17-3.18 (name "a"))))))
	(d-let
		(p-assign @10.1-10.6 (ident "main!"))
		(e-closure @10.9-10.15
			(e-lambda @10.9-10.15
				(args
					(p-underscore @10.10-10.11))
				(e-empty_record @10.13-10.15)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.9 (type "a -> a"))
		(patt @10.1-10.6 (type "_arg -> {}")))
	(expressions
		(expr @4.12-8.2 (type "a -> a"))
		(expr @10.9-10.15 (type "_arg -> {}"))))
~~~
