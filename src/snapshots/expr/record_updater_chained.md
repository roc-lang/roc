# META
~~~ini
description=Chained record updater expressions
type=file
~~~
# SOURCE
~~~roc
module [person, final]

person = { name: "Alice", age: 30, city: "Boston" }
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:22),CloseSquare(1:22-1:23),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),OpenCurly(3:10-3:11),LowerIdent(3:12-3:16),OpColon(3:16-3:17),StringStart(3:18-3:19),StringPart(3:19-3:24),StringEnd(3:24-3:25),Comma(3:25-3:26),LowerIdent(3:27-3:30),OpColon(3:30-3:31),Int(3:32-3:34),Comma(3:34-3:35),LowerIdent(3:36-3:40),OpColon(3:40-3:41),StringStart(3:42-3:43),StringPart(3:43-3:49),StringEnd(3:49-3:50),CloseCurly(3:51-3:52),Newline(1:1-1:1),
LowerIdent(4:1-4:12),OpAssign(4:13-4:14),OpenCurly(4:15-4:16),DoubleDot(4:17-4:19),LowerIdent(4:19-4:25),Comma(4:25-4:26),LowerIdent(4:27-4:30),OpColon(4:30-4:31),Int(4:32-4:34),CloseCurly(4:35-4:36),Newline(1:1-1:1),
LowerIdent(5:1-5:9),OpAssign(5:10-5:11),OpenCurly(5:12-5:13),DoubleDot(5:14-5:16),LowerIdent(5:16-5:27),Comma(5:27-5:28),LowerIdent(5:29-5:33),OpColon(5:33-5:34),StringStart(5:35-5:36),StringPart(5:36-5:44),StringEnd(5:44-5:45),CloseCurly(5:46-5:47),Newline(1:1-1:1),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),OpenCurly(6:9-6:10),DoubleDot(6:11-6:13),LowerIdent(6:13-6:21),Comma(6:21-6:22),LowerIdent(6:23-6:27),OpColon(6:27-6:28),StringStart(6:29-6:30),StringPart(6:30-6:41),StringEnd(6:41-6:42),Comma(6:42-6:43),LowerIdent(6:44-6:47),OpColon(6:47-6:48),Int(6:49-6:51),CloseCurly(6:52-6:53),EndOfFile(6:53-6:53),
~~~
# PARSE
~~~clojure
(file @1.1-6.53
	(module @1.1-1.23
		(exposes @1.8-1.23
			(exposed-lower-ident (text "person"))
			(exposed-lower-ident (text "final"))))
	(statements
		(s-decl @3.1-3.52
			(p-ident @3.1-3.7 (raw "person"))
			(e-record @3.10-3.52
				(field (field "name") (optional false)
					(e-string @3.18-3.25
						(e-string-part @3.19-3.24 (raw "Alice"))))
				(field (field "age") (optional false)
					(e-int @3.32-3.34 (raw "30")))
				(field (field "city") (optional false)
					(e-string @3.42-3.50
						(e-string-part @3.43-3.49 (raw "Boston"))))))
		(s-decl @4.1-4.36
			(p-ident @4.1-4.12 (raw "updated_one"))
			(e-record @4.15-4.36
				(ext
					(e-ident @4.19-4.25 (raw "person")))
				(field (field "age") (optional false)
					(e-int @4.32-4.34 (raw "31")))))
		(s-decl @5.1-5.47
			(p-ident @5.1-5.9 (raw "updated2"))
			(e-record @5.12-5.47
				(ext
					(e-ident @5.16-5.27 (raw "updated_one")))
				(field (field "city") (optional false)
					(e-string @5.35-5.45
						(e-string-part @5.36-5.44 (raw "New York"))))))
		(s-decl @6.1-6.53
			(p-ident @6.1-6.6 (raw "final"))
			(e-record @6.9-6.53
				(ext
					(e-ident @6.13-6.21 (raw "updated2")))
				(field (field "name") (optional false)
					(e-string @6.29-6.42
						(e-string-part @6.30-6.41 (raw "Alice Smith"))))
				(field (field "age") (optional false)
					(e-int @6.49-6.51 (raw "32")))))))
~~~
# FORMATTED
~~~roc
module [person, final]

person = {name: "Alice", age: 30, city: "Boston"}
updated_one = {..person, age: 31}
updated2 = {..updated_one, city: "New York"}
final = {..updated2, name: "Alice Smith", age: 32}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.7 (ident "person"))
		(e-record @3.10-3.52
			(fields
				(field (name "name")
					(e-string @3.18-3.25
						(e-literal @3.19-3.24 (string "Alice"))))
				(field (name "age")
					(e-int @3.32-3.34 (value "30")))
				(field (name "city")
					(e-string @3.42-3.50
						(e-literal @3.43-3.49 (string "Boston")))))))
	(d-let
		(p-assign @4.1-4.12 (ident "updated_one"))
		(e-record @4.15-4.36
			(ext
				(e-lookup-local @4.19-4.25
					(p-assign @3.1-3.7 (ident "person"))))
			(fields
				(field (name "age")
					(e-int @4.32-4.34 (value "31"))))))
	(d-let
		(p-assign @5.1-5.9 (ident "updated2"))
		(e-record @5.12-5.47
			(ext
				(e-lookup-local @5.16-5.27
					(p-assign @4.1-4.12 (ident "updated_one"))))
			(fields
				(field (name "city")
					(e-string @5.35-5.45
						(e-literal @5.36-5.44 (string "New York")))))))
	(d-let
		(p-assign @6.1-6.6 (ident "final"))
		(e-record @6.9-6.53
			(ext
				(e-lookup-local @6.13-6.21
					(p-assign @5.1-5.9 (ident "updated2"))))
			(fields
				(field (name "name")
					(e-string @6.29-6.42
						(e-literal @6.30-6.41 (string "Alice Smith"))))
				(field (name "age")
					(e-int @6.49-6.51 (value "32")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "{ name: Str, age: Num(*), city: Str }"))
		(patt @4.1-4.12 (type "{ age: Num(*) }"))
		(patt @5.1-5.9 (type "{ city: Str }"))
		(patt @6.1-6.6 (type "{ name: Str, age: Num(*) }")))
	(expressions
		(expr @3.10-3.52 (type "{ name: Str, age: Num(*), city: Str }"))
		(expr @4.15-4.36 (type "{ age: Num(*) }"))
		(expr @5.12-5.47 (type "{ city: Str }"))
		(expr @6.9-6.53 (type "{ name: Str, age: Num(*) }"))))
~~~
