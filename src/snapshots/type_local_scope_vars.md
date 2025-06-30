# META
~~~ini
description=Function-local type variables in separate scopes
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

outer : a -> a
outer = |x| {
    inner : b -> b
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:6),OpColon(3:7-3:8),LowerIdent(3:9-3:10),OpArrow(3:11-3:13),LowerIdent(3:14-3:15),Newline(1:1-1:1),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpBar(4:9-4:10),LowerIdent(4:10-4:11),OpBar(4:11-4:12),OpenCurly(4:13-4:14),Newline(1:1-1:1),
LowerIdent(5:5-5:10),OpColon(5:11-5:12),LowerIdent(5:13-5:14),OpArrow(5:15-5:17),LowerIdent(5:18-5:19),Newline(1:1-1:1),
LowerIdent(6:5-6:10),OpAssign(6:11-6:12),OpBar(6:13-6:14),LowerIdent(6:14-6:15),OpBar(6:15-6:16),LowerIdent(6:17-6:18),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:5-8:10),NoSpaceOpenRound(8:10-8:11),LowerIdent(8:11-8:12),CloseRound(8:12-8:13),Newline(1:1-1:1),
CloseCurly(9:1-9:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(11:1-11:6),OpAssign(11:7-11:8),OpBar(11:9-11:10),Underscore(11:10-11:11),OpBar(11:11-11:12),OpenCurly(11:13-11:14),CloseCurly(11:14-11:15),EndOfFile(11:15-11:15),
~~~
# PARSE
~~~clojure
(file @1.1-11.15
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
		(s-type-anno @3.1-4.6 (name "outer")
			(ty-fn @3.9-3.15
				(ty-var @3.9-3.10 (raw "a"))
				(ty-var @3.14-3.15 (raw "a"))))
		(s-decl @4.1-9.2
			(p-ident @4.1-4.6 (raw "outer"))
			(e-lambda @4.9-9.2
				(args
					(p-ident @4.10-4.11 (raw "x")))
				(e-block @4.13-9.2
					(statements
						(s-type-anno @5.5-6.10 (name "inner")
							(ty-fn @5.13-5.19
								(ty-var @5.13-5.14 (raw "b"))
								(ty-var @5.18-5.19 (raw "b"))))
						(s-decl @6.5-6.18
							(p-ident @6.5-6.10 (raw "inner"))
							(e-lambda @6.13-6.18
								(args
									(p-ident @6.14-6.15 (raw "y")))
								(e-ident @6.17-6.18 (qaul "") (raw "y"))))
						(e-apply @8.5-8.13
							(e-ident @8.5-8.10 (qaul "") (raw "inner"))
							(e-ident @8.11-8.12 (qaul "") (raw "x")))))))
		(s-decl @11.1-11.15
			(p-ident @11.1-11.6 (raw "main!"))
			(e-lambda @11.9-11.15
				(args
					(p-underscore))
				(e-record @11.13-11.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

outer : a -> a
outer = |x| {
	inner : b -> b
	inner = |y| y

	inner(x)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.6 (ident "outer"))
		(e-lambda @4.9-9.2
			(args
				(p-assign @4.10-4.11 (ident "x")))
			(e-block @4.13-9.2
				(s-type-anno @5.5-6.10 (name "inner")
					(ty-fn @5.13-5.19 (effectful false)
						(ty-var @5.13-5.14 (name "b"))
						(ty-var @5.18-5.19 (name "b"))))
				(s-let @6.5-6.18
					(p-assign @6.5-6.10 (ident "inner"))
					(e-lambda @6.13-6.18
						(args
							(p-assign @6.14-6.15 (ident "y")))
						(e-lookup-local @6.17-6.18
							(pattern @6.14-6.15))))
				(e-call @8.5-8.13
					(e-lookup-local @8.5-8.10
						(pattern @6.5-6.10))
					(e-lookup-local @8.11-8.12
						(pattern @4.10-4.11)))))
		(annotation @4.1-4.6
			(declared-type
				(ty-fn @3.9-3.15 (effectful false)
					(ty-var @3.9-3.10 (name "a"))
					(ty-var @3.14-3.15 (name "a"))))))
	(d-let
		(p-assign @11.1-11.6 (ident "main!"))
		(e-lambda @11.9-11.15
			(args
				(p-underscore @11.10-11.11))
			(e-empty_record @11.13-11.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.6 (type "a -> a"))
		(patt @11.1-11.6 (type "* ? {}")))
	(expressions
		(expr @4.9-9.2 (type "a -> a"))
		(expr @11.9-11.15 (type "* ? {}"))))
~~~
