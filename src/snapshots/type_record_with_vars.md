# META
~~~ini
description=Record with type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field: a, other: b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),OpenCurly(3:12-3:13),LowerIdent(3:14-3:19),OpColon(3:19-3:20),LowerIdent(3:21-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:29),OpColon(3:29-3:30),LowerIdent(3:31-3:32),CloseCurly(3:33-3:34),OpArrow(3:35-3:37),LowerIdent(3:38-3:39),Newline(1:1-1:1),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:19),OpBar(4:19-4:20),LowerIdent(4:21-4:27),NoSpaceDotLowerIdent(4:27-4:33),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),EndOfFile(6:15-6:15),
~~~
# PARSE
~~~clojure
(file @1-1-6-15
	(app @1-1-1-53
		(provides @1-6-1-12
			(exposed-lower-ident (text "main!")))
		(record-field @1-15-1-53 (name "pf")
			(e-string @1-28-1-51
				(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))
		(packages @1-13-1-53
			(record-field @1-15-1-53 (name "pf")
				(e-string @1-28-1-51
					(e-string-part @1-29-1-50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno @3-1-4-9 (name "getField")
			(ty-fn @3-12-3-39
				(ty-record @3-12-3-34
					(anno-record-field @3-14-3-23 (name "field")
						(ty-var @3-21-3-22 (raw "a")))
					(anno-record-field @3-24-3-34 (name "other")
						(ty-var @3-31-3-32 (raw "b"))))
				(ty-var @3-38-3-39 (raw "a"))))
		(s-decl @4-1-6-6
			(p-ident @4-1-4-9 (raw "getField"))
			(e-lambda @4-12-6-6
				(args
					(p-ident @4-13-4-19 (raw "record")))
				(e-field-access @4-21-6-6
					(e-ident @4-21-4-27 (qaul "") (raw "record"))
					(e-ident @4-27-4-33 (qaul "") (raw ".field")))))
		(s-decl @6-1-6-15
			(p-ident @6-1-6-6 (raw "main!"))
			(e-lambda @6-9-6-15
				(args
					(p-underscore))
				(e-record @6-13-6-15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field : a, other : b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 100)
		(p-assign @4-1-4-9 (ident "getField") (id 83))
		(e-lambda @4-12-6-6 (id 88)
			(args
				(p-assign @4-13-4-19 (ident "record") (id 84)))
			(e-dot-access @4-21-6-6 (field "field")
				(receiver
					(e-lookup-local @4-21-4-27
						(pattern (id 84))))))
		(annotation @4-1-4-9 (signature 98) (id 99)
			(declared-type
				(ty-fn @3-12-3-39 (effectful false)
					(ty-record @3-12-3-34
						(field (field "field")
							(ty-var @3-21-3-22 (name "a")))
						(field (field "other")
							(ty-var @3-31-3-32 (name "b"))))
					(ty-var @3-38-3-39 (name "a"))))))
	(d-let (id 106)
		(p-assign @6-1-6-6 (ident "main!") (id 101))
		(e-lambda @6-9-6-15 (id 105)
			(args
				(p-underscore @6-10-6-11 (id 102)))
			(e-empty_record @6-13-6-15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "getField") (def_var 100) (type "{ field: a, other: b } -> a"))
		(d_assign (name "main!") (def_var 106) (type "* ? {}")))
	(expressions
		(expr @4-12-6-6 (type "{ field: a, other: b } -> a"))
		(expr @6-9-6-15 (type "* ? {}"))))
~~~
