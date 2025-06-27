# META
~~~ini
description=Basic type application canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one","two","three"])
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:12),OpColon(3:13-3:14),UpperIdent(3:15-3:19),NoSpaceOpenRound(3:19-3:20),UpperIdent(3:20-3:23),CloseRound(3:23-3:24),OpArrow(3:25-3:27),UpperIdent(3:28-3:31),Newline(1:1-1:1),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpBar(4:15-4:16),LowerIdent(4:16-4:20),OpBar(4:20-4:21),LowerIdent(4:22-4:26),NoSpaceDotLowerIdent(4:26-4:30),NoSpaceOpenRound(4:30-4:31),CloseRound(4:31-4:32),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpBar(6:9-6:10),Underscore(6:10-6:11),OpBar(6:11-6:12),LowerIdent(6:13-6:24),NoSpaceOpenRound(6:24-6:25),OpenSquare(6:25-6:26),StringStart(6:26-6:27),StringPart(6:27-6:30),StringEnd(6:30-6:31),Comma(6:31-6:32),StringStart(6:32-6:33),StringPart(6:33-6:36),StringEnd(6:36-6:37),Comma(6:37-6:38),StringStart(6:38-6:39),StringPart(6:39-6:44),StringEnd(6:44-6:45),CloseSquare(6:45-6:46),CloseRound(6:46-6:47),EndOfFile(6:47-6:47),
~~~
# PARSE
~~~clojure
(file @1-1-6-47
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
		(s-type-anno @3-1-4-12 (name "processList")
			(ty-fn @3-15-3-31
				(ty-apply @3-15-3-24
					(ty (name "List"))
					(ty (name "Str")))
				(ty (name "U64"))))
		(s-decl @4-1-6-6
			(p-ident @4-1-4-12 (raw "processList"))
			(e-lambda @4-15-6-6
				(args
					(p-ident @4-16-4-20 (raw "list")))
				(e-field-access @4-22-6-6
					(e-ident @4-22-4-26 (qaul "") (raw "list"))
					(e-apply @4-26-4-32
						(e-ident @4-26-4-30 (qaul "") (raw ".len"))))))
		(s-decl @6-1-6-47
			(p-ident @6-1-6-6 (raw "main!"))
			(e-lambda @6-9-6-47
				(args
					(p-underscore))
				(e-apply @6-13-6-47
					(e-ident @6-13-6-24 (qaul "") (raw "processList"))
					(e-list @6-25-6-46
						(e-string @6-26-6-31
							(e-string-part @6-27-6-30 (raw "one")))
						(e-string @6-32-6-37
							(e-string-part @6-33-6-36 (raw "two")))
						(e-string @6-38-6-45
							(e-string-part @6-39-6-44 (raw "three")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

processList : List(Str) -> U64
processList = |list| list.len()

main! = |_| processList(["one", "two", "three"])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 86)
		(p-assign @4-1-4-12 (ident "processList") (id 76))
		(e-lambda @4-15-6-6 (id 80)
			(args
				(p-assign @4-16-4-20 (ident "list") (id 77)))
			(e-dot-access @4-22-6-6 (field "len")
				(receiver
					(e-lookup-local @4-22-4-26
						(pattern (id 77))))
				(args)))
		(annotation @4-1-4-12 (signature 84) (id 85)
			(declared-type
				(ty-fn @3-15-3-31 (effectful false)
					(ty-apply @3-15-3-24 (symbol "List")
						(ty @3-20-3-23 (name "Str")))
					(ty @3-28-3-31 (name "U64"))))))
	(d-let (id 100)
		(p-assign @6-1-6-6 (ident "main!") (id 87))
		(e-lambda @6-9-6-47 (id 99)
			(args
				(p-underscore @6-10-6-11 (id 88)))
			(e-call @6-13-6-47
				(e-lookup-local @6-13-6-24
					(pattern (id 76)))
				(e-list @6-25-6-46 (elem-var 96)
					(elems
						(e-string @6-26-6-31
							(e-literal @6-27-6-30 (string "one")))
						(e-string @6-32-6-37
							(e-literal @6-33-6-36 (string "two")))
						(e-string @6-38-6-45
							(e-literal @6-39-6-44 (string "three")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "processList") (type "*"))
		(def (name "main!") (type "*")))
	(expressions
		(expr @4-15-6-6 (type "*"))
		(expr @6-9-6-47 (type "*"))))
~~~