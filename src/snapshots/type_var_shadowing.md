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
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:40),
LowerIdent(4:1-4:6),OpColon(4:7-4:8),LowerIdent(4:9-4:10),OpArrow(4:11-4:13),LowerIdent(4:14-4:15),Newline(1:1-1:1),
LowerIdent(5:1-5:6),OpAssign(5:7-5:8),OpBar(5:9-5:10),LowerIdent(5:10-5:11),OpBar(5:11-5:12),OpenCurly(5:13-5:14),Newline(1:1-1:1),
Newline(6:6-6:56),
LowerIdent(7:5-7:10),OpColon(7:11-7:12),LowerIdent(7:13-7:14),OpArrow(7:15-7:17),LowerIdent(7:18-7:19),Newline(1:1-1:1),
LowerIdent(8:5-8:10),OpAssign(8:11-8:12),OpBar(8:13-8:14),LowerIdent(8:14-8:15),OpBar(8:15-8:16),LowerIdent(8:17-8:18),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(10:5-10:10),NoSpaceOpenRound(10:10-10:11),LowerIdent(10:11-10:12),CloseRound(10:12-10:13),Newline(1:1-1:1),
CloseCurly(11:1-11:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(13:1-13:6),OpAssign(13:7-13:8),OpBar(13:9-13:10),Underscore(13:10-13:11),OpBar(13:11-13:12),OpenCurly(13:13-13:14),CloseCurly(13:14-13:15),EndOfFile(13:15-13:15),
~~~
# PARSE
~~~clojure
(file @1-1-13-15
	(app @1-1-1-57
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-57 (name "pf")
			(e-string @1-28-1-55
				(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))
		(packages @1-13-1-57
			(record-field @1-15-1-57 (name "pf")
				(e-string @1-28-1-55
					(e-string-part @1-29-1-54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4-1-5-6 (name "outer")
			(ty-fn @4-9-4-15
				(ty-var @4-9-4-10 (raw "a"))
				(ty-var @4-14-4-15 (raw "a"))))
		(s-decl @5-1-11-2
			(p-ident @5-1-5-6 (raw "outer"))
			(e-lambda @5-9-11-2
				(args
					(p-ident @5-10-5-11 (raw "x")))
				(e-block @5-13-11-2
					(statements
						(s-type-anno @7-5-8-10 (name "inner")
							(ty-fn @7-13-7-19
								(ty-var @7-13-7-14 (raw "a"))
								(ty-var @7-18-7-19 (raw "a"))))
						(s-decl @8-5-8-18
							(p-ident @8-5-8-10 (raw "inner"))
							(e-lambda @8-13-8-18
								(args
									(p-ident @8-14-8-15 (raw "y")))
								(e-ident @8-17-8-18 (qaul "") (raw "y"))))
						(e-apply @10-5-10-13
							(e-ident @10-5-10-10 (qaul "") (raw "inner"))
							(e-ident @10-11-10-12 (qaul "") (raw "x")))))))
		(s-decl @13-1-13-15
			(p-ident @13-1-13-6 (raw "main!"))
			(e-lambda @13-9-13-15
				(args
					(p-underscore))
				(e-record @13-13-13-15)))))
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
	(d-let (id 106)
		(p-assign @5-1-5-6 (ident "outer") (id 77))
		(e-lambda @5-9-11-2 (id 98)
			(args
				(p-assign @5-10-5-11 (ident "x") (id 78)))
			(e-block @5-13-11-2
				(s-type-anno @7-5-8-10 (name "inner")
					(ty-fn @7-13-7-19 (effectful false)
						(ty-var @7-13-7-14 (name "a"))
						(ty-var @7-18-7-19 (name "a"))))
				(s-let @8-5-8-18
					(p-assign @8-5-8-10 (ident "inner") (id 86))
					(e-lambda @8-13-8-18 (id 90)
						(args
							(p-assign @8-14-8-15 (ident "y") (id 87)))
						(e-lookup-local @8-17-8-18
							(pattern (id 87)))))
				(e-call @10-5-10-13
					(e-lookup-local @10-5-10-10
						(pattern (id 86)))
					(e-lookup-local @10-11-10-12
						(pattern (id 78))))))
		(annotation @5-1-5-6 (signature 104) (id 105)
			(declared-type
				(ty-fn @4-9-4-15 (effectful false)
					(ty-var @4-9-4-10 (name "a"))
					(ty-var @4-14-4-15 (name "a"))))))
	(d-let (id 112)
		(p-assign @13-1-13-6 (ident "main!") (id 107))
		(e-lambda @13-9-13-15 (id 111)
			(args
				(p-underscore @13-10-13-11 (id 108)))
			(e-empty_record @13-13-13-15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "outer") (def_var 106) (type "a -> a"))
		(d_assign (name "main!") (def_var 112) (type "* ? {}")))
	(expressions
		(expr @5-9-11-2 (type "a -> a"))
		(expr @13-9-13-15 (type "* ? {}"))))
~~~
