# META
~~~ini
description=Type variable shadowing produces warning but is allowed
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Outer function with type variable 'a'
outer : a -> a
outer = |x| {
    # Inner function shadows outer 'a' with its own 'a'
    inner : a -> a
    inner = |y| y

    inner(x)
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
LowerIdent(4:1-4:6),OpColon(4:7-4:8),LowerIdent(4:9-4:10),OpArrow(4:11-4:13),LowerIdent(4:14-4:15),
LowerIdent(5:1-5:6),OpAssign(5:7-5:8),OpBar(5:9-5:10),LowerIdent(5:10-5:11),OpBar(5:11-5:12),OpenCurly(5:13-5:14),
LowerIdent(7:5-7:10),OpColon(7:11-7:12),LowerIdent(7:13-7:14),OpArrow(7:15-7:17),LowerIdent(7:18-7:19),
LowerIdent(8:5-8:10),OpAssign(8:11-8:12),OpBar(8:13-8:14),LowerIdent(8:14-8:15),OpBar(8:15-8:16),LowerIdent(8:17-8:18),
LowerIdent(10:5-10:10),NoSpaceOpenRound(10:10-10:11),LowerIdent(10:11-10:12),CloseRound(10:12-10:13),
CloseCurly(11:1-11:2),
LowerIdent(13:1-13:6),OpAssign(13:7-13:8),OpBar(13:9-13:10),Underscore(13:10-13:11),OpBar(13:11-13:12),OpenCurly(13:13-13:14),CloseCurly(13:14-13:15),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @1.1-13.15
	(app @1.1-1.57
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.55 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.55 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-4.15 (name "outer")
			(ty-fn @4.9-4.15
				(ty-var @4.9-4.10 (raw "a"))
				(ty-var @4.14-4.15 (raw "a"))))
		(s-decl @5.1-11.2
			(p-ident @5.1-5.6 (raw "outer"))
			(e-lambda @5.9-11.2
				(args
					(p-ident @5.10-5.11 (raw "x")))
				(e-block @5.13-11.2
					(statements
						(s-type-anno @7.5-7.19 (name "inner")
							(ty-fn @7.13-7.19
								(ty-var @7.13-7.14 (raw "a"))
								(ty-var @7.18-7.19 (raw "a"))))
						(s-decl @8.5-8.18
							(p-ident @8.5-8.10 (raw "inner"))
							(e-lambda @8.13-8.18
								(args
									(p-ident @8.14-8.15 (raw "y")))
								(e-ident @8.17-8.18 (raw "y"))))
						(e-apply @10.5-10.13
							(e-ident @10.5-10.10 (raw "inner"))
							(e-ident @10.11-10.12 (raw "x")))))))
		(s-decl @13.1-13.15
			(p-ident @13.1-13.6 (raw "main!"))
			(e-lambda @13.9-13.15
				(args
					(p-underscore))
				(e-record @13.13-13.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

# Outer function with type variable 'a'
outer : a -> a
outer = |x| {
	# Inner function shadows outer 'a' with its own 'a'
	inner : a -> a
	inner = |y| y

	inner(x)
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.6 (ident "outer"))
		(e-lambda @5.9-11.2
			(args
				(p-assign @5.10-5.11 (ident "x")))
			(e-block @5.13-11.2
				(s-let @8.5-8.18
					(p-assign @8.5-8.10 (ident "inner"))
					(e-lambda @8.13-8.18
						(args
							(p-assign @8.14-8.15 (ident "y")))
						(e-lookup-local @8.17-8.18
							(p-assign @8.14-8.15 (ident "y")))))
				(e-call @10.5-10.13
					(e-lookup-local @10.11-10.12
						(p-assign @5.10-5.11 (ident "x"))))))
		(annotation @5.1-5.6
			(declared-type
				(ty-fn @4.9-4.15 (effectful false)
					(ty-rigid-var @4.9-4.10 (name "a"))
					(ty-rigid-var @4.9-4.10 (name "a"))))))
	(d-let
		(p-assign @13.1-13.6 (ident "main!"))
		(e-lambda @13.9-13.15
			(args
				(p-underscore @13.10-13.11))
			(e-empty_record @13.13-13.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.6 (type "a -> a"))
		(patt @13.1-13.6 (type "_arg -> {}")))
	(expressions
		(expr @5.9-11.2 (type "a -> a"))
		(expr @13.9-13.15 (type "_arg -> {}"))))
~~~
