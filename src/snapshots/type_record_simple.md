# META
~~~ini
description=Simple record type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

getName : { name: Str, age: U64 } -> Str
getName = |person| person.name

main! = |_| {}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenCurly(3:11-3:12),LowerIdent(3:13-3:17),OpColon(3:17-3:18),UpperIdent(3:19-3:22),Comma(3:22-3:23),LowerIdent(3:24-3:27),OpColon(3:27-3:28),UpperIdent(3:29-3:32),CloseCurly(3:33-3:34),OpArrow(3:35-3:37),UpperIdent(3:38-3:41),Newline(1:1-1:1),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:18),OpBar(4:18-4:19),LowerIdent(4:20-4:26),NoSpaceDotLowerIdent(4:26-4:31),Newline(1:1-1:1),
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
		(s-type-anno @3-1-4-8 (name "getName")
			(ty-fn @3-11-3-41
				(ty-record @3-11-3-34
					(anno-record-field @3-13-3-23 (name "name")
						(ty (name "Str")))
					(anno-record-field @3-24-3-34 (name "age")
						(ty (name "U64"))))
				(ty (name "Str"))))
		(s-decl @4-1-6-6
			(p-ident @4-1-4-8 (raw "getName"))
			(e-lambda @4-11-6-6
				(args
					(p-ident @4-12-4-18 (raw "person")))
				(e-field-access @4-20-6-6
					(e-binop @4-20-6-6 (op "app")
						(e-ident @4-20-4-26 (qaul "") (raw "person"))
						(e-ident @4-26-4-31 (qaul "") (raw ".name"))))))
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

getName : { name : Str, age : U64 } -> Str
getName = |person| person.name

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 89)
		(p-assign @4-1-4-8 (ident "getName") (id 79))
		(e-lambda @4-11-6-6 (id 83)
			(args
				(p-assign @4-12-4-18 (ident "person") (id 80)))
			(e-dot-access @4-20-6-6 (field "name")
				(receiver
					(e-lookup-local @4-20-4-26
						(pattern (id 80))))))
		(annotation @4-1-4-8 (signature 87) (id 88)
			(declared-type
				(ty-fn @3-11-3-41 (effectful false)
					(ty-record @3-11-3-34
						(field (field "name")
							(ty @3-19-3-22 (name "Str")))
						(field (field "age")
							(ty @3-29-3-32 (name "U64"))))
					(ty @3-38-3-41 (name "Str"))))))
	(d-let (id 94)
		(p-assign @6-1-6-6 (ident "main!") (id 90))
		(e-lambda @6-9-6-15 (id 93)
			(args
				(p-underscore @6-10-6-11 (id 91)))
			(e-empty_record @6-13-6-15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "getName") (type "*"))
		(def (name "main!") (type "*")))
	(expressions
		(expr @4-11-6-6 (type "*"))
		(expr @6-9-6-15 (type "*"))))
~~~