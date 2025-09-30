# META
~~~ini
description=Record with type variables in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field: a, other: _b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),OpenCurly(3:12-3:13),LowerIdent(3:14-3:19),OpColon(3:19-3:20),LowerIdent(3:21-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:29),OpColon(3:29-3:30),NamedUnderscore(3:31-3:33),CloseCurly(3:34-3:35),OpArrow(3:36-3:38),LowerIdent(3:39-3:40),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:19),OpBar(4:19-4:20),LowerIdent(4:21-4:27),NoSpaceDotLowerIdent(4:27-4:33),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),OpenCurly(6:13-6:14),CloseCurly(6:14-6:15),
EndOfFile(7:1-7:1),
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
		(s-type-anno @3.1-3.40 (name "getField")
			(ty-fn @3.12-3.40
				(ty-record @3.12-3.35
					(anno-record-field @3.14-3.22 (name "field")
						(ty-var @3.21-3.22 (raw "a")))
					(anno-record-field @3.24-3.33 (name "other")
						(underscore-ty-var @3.31-3.33 (raw "_b"))))
				(ty-var @3.39-3.40 (raw "a"))))
		(s-decl @4.1-4.33
			(p-ident @4.1-4.9 (raw "getField"))
			(e-lambda @4.12-4.33
				(args
					(p-ident @4.13-4.19 (raw "record")))
				(e-field-access @4.21-4.33
					(e-ident @4.21-4.27 (raw "record"))
					(e-ident @4.27-4.33 (raw "field")))))
		(s-decl @6.1-6.15
			(p-ident @6.1-6.6 (raw "main!"))
			(e-lambda @6.9-6.15
				(args
					(p-underscore))
				(e-record @6.13-6.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getField : { field : a, other : _b } -> a
getField = |record| record.field

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.9 (ident "getField"))
		(e-lambda @4.12-4.33
			(args
				(p-assign @4.13-4.19 (ident "record")))
			(e-dot-access @4.21-4.33 (field "field")
				(receiver
					(e-lookup-local @4.21-4.27
						(p-assign @4.13-4.19 (ident "record"))))))
		(annotation @4.1-4.9
			(declared-type
				(ty-fn @3.12-3.40 (effectful false)
					(ty-record @3.12-3.35
						(field (field "field")
							(ty-rigid-var @3.21-3.22 (name "a")))
						(field (field "other")
							(ty-rigid-var @3.31-3.33 (name "_b"))))
					(ty-rigid-var-lookup (ty-rigid-var @3.21-3.22 (name "a")))))))
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
		(patt @4.1-4.9 (type "{ field: a, other: _b } -> a"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.12-4.33 (type "{ field: a, other: _b } -> a"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
