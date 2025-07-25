# META
~~~ini
description=Simple record type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

get_name : { name: Str, age: U64 } -> Str
get_name = |person| person.name

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),OpenCurly(3:12-3:13),LowerIdent(3:14-3:18),OpColon(3:18-3:19),UpperIdent(3:20-3:23),Comma(3:23-3:24),LowerIdent(3:25-3:28),OpColon(3:28-3:29),UpperIdent(3:30-3:33),CloseCurly(3:34-3:35),OpArrow(3:36-3:38),UpperIdent(3:39-3:42),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),OpBar(4:12-4:13),LowerIdent(4:13-4:19),OpBar(4:19-4:20),LowerIdent(4:21-4:27),NoSpaceDotLowerIdent(4:27-4:32),
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
		(s-type-anno @3.1-3.42 (name "get_name")
			(ty-fn @3.12-3.42
				(ty-record @3.12-3.35
					(anno-record-field @3.14-3.23 (name "name")
						(ty @3.20-3.23 (name "Str")))
					(anno-record-field @3.25-3.33 (name "age")
						(ty @3.30-3.33 (name "U64"))))
				(ty @3.39-3.42 (name "Str"))))
		(s-decl @4.1-4.32
			(p-ident @4.1-4.9 (raw "get_name"))
			(e-lambda @4.12-4.32
				(args
					(p-ident @4.13-4.19 (raw "person")))
				(e-field-access @4.21-4.32
					(e-ident @4.21-4.27 (raw "person"))
					(e-ident @4.27-4.32 (raw "name")))))
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

get_name : { name : Str, age : U64 } -> Str
get_name = |person| person.name

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.9 (ident "get_name"))
		(e-closure @4.12-4.32
			(e-lambda @4.12-4.32
				(args
					(p-assign @4.13-4.19 (ident "person")))
				(e-dot-access @4.21-4.32 (field "name")
					(receiver
						(e-lookup-local @4.21-4.27
							(p-assign @4.13-4.19 (ident "person")))))))
		(annotation @4.1-4.9
			(declared-type
				(ty-fn @3.12-3.42 (effectful false)
					(ty-record @3.12-3.35
						(field (field "name")
							(ty @3.20-3.23 (name "Str")))
						(field (field "age")
							(ty @3.30-3.33 (name "U64"))))
					(ty @3.39-3.42 (name "Str"))))))
	(d-let
		(p-assign @6.1-6.6 (ident "main!"))
		(e-closure @6.9-6.15
			(e-lambda @6.9-6.15
				(args
					(p-underscore @6.10-6.11))
				(e-empty_record @6.13-6.15)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.9 (type "{ name: Str, age: U64 } -> Str"))
		(patt @6.1-6.6 (type "_arg -> {}")))
	(expressions
		(expr @4.12-4.32 (type "{ name: Str, age: U64 } -> Str"))
		(expr @6.9-6.15 (type "_arg -> {}"))))
~~~
