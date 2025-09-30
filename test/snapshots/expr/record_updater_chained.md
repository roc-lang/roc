# META
~~~ini
description=Chained record updater expressions
type=file
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30, city: "Boston" }
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
~~~
# EXPECTED
MISSING MAIN! FUNCTION - record_updater_chained.md:1:1:4:53
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**record_updater_chained.md:1:1:4:53:**
```roc
person = { name: "Alice", age: 30, city: "Boston" }
updated_one = { ..person, age: 31 }
updated2 = { ..updated_one, city: "New York" }
final = { ..updated2, name: "Alice Smith", age: 32 }
```


# TOKENS
~~~zig
LowerIdent(1:1-1:7),OpAssign(1:8-1:9),OpenCurly(1:10-1:11),LowerIdent(1:12-1:16),OpColon(1:16-1:17),StringStart(1:18-1:19),StringPart(1:19-1:24),StringEnd(1:24-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:30),OpColon(1:30-1:31),Int(1:32-1:34),Comma(1:34-1:35),LowerIdent(1:36-1:40),OpColon(1:40-1:41),StringStart(1:42-1:43),StringPart(1:43-1:49),StringEnd(1:49-1:50),CloseCurly(1:51-1:52),
LowerIdent(2:1-2:12),OpAssign(2:13-2:14),OpenCurly(2:15-2:16),DoubleDot(2:17-2:19),LowerIdent(2:19-2:25),Comma(2:25-2:26),LowerIdent(2:27-2:30),OpColon(2:30-2:31),Int(2:32-2:34),CloseCurly(2:35-2:36),
LowerIdent(3:1-3:9),OpAssign(3:10-3:11),OpenCurly(3:12-3:13),DoubleDot(3:14-3:16),LowerIdent(3:16-3:27),Comma(3:27-3:28),LowerIdent(3:29-3:33),OpColon(3:33-3:34),StringStart(3:35-3:36),StringPart(3:36-3:44),StringEnd(3:44-3:45),CloseCurly(3:46-3:47),
LowerIdent(4:1-4:6),OpAssign(4:7-4:8),OpenCurly(4:9-4:10),DoubleDot(4:11-4:13),LowerIdent(4:13-4:21),Comma(4:21-4:22),LowerIdent(4:23-4:27),OpColon(4:27-4:28),StringStart(4:29-4:30),StringPart(4:30-4:41),StringEnd(4:41-4:42),Comma(4:42-4:43),LowerIdent(4:44-4:47),OpColon(4:47-4:48),Int(4:49-4:51),CloseCurly(4:52-4:53),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.53
	(type-module @1.1-1.7)
	(statements
		(s-decl @1.1-1.52
			(p-ident @1.1-1.7 (raw "person"))
			(e-record @1.10-1.52
				(field (field "name")
					(e-string @1.18-1.25
						(e-string-part @1.19-1.24 (raw "Alice"))))
				(field (field "age")
					(e-int @1.32-1.34 (raw "30")))
				(field (field "city")
					(e-string @1.42-1.50
						(e-string-part @1.43-1.49 (raw "Boston"))))))
		(s-decl @2.1-2.36
			(p-ident @2.1-2.12 (raw "updated_one"))
			(e-record @2.15-2.36
				(ext
					(e-ident @2.19-2.25 (raw "person")))
				(field (field "age")
					(e-int @2.32-2.34 (raw "31")))))
		(s-decl @3.1-3.47
			(p-ident @3.1-3.9 (raw "updated2"))
			(e-record @3.12-3.47
				(ext
					(e-ident @3.16-3.27 (raw "updated_one")))
				(field (field "city")
					(e-string @3.35-3.45
						(e-string-part @3.36-3.44 (raw "New York"))))))
		(s-decl @4.1-4.53
			(p-ident @4.1-4.6 (raw "final"))
			(e-record @4.9-4.53
				(ext
					(e-ident @4.13-4.21 (raw "updated2")))
				(field (field "name")
					(e-string @4.29-4.42
						(e-string-part @4.30-4.41 (raw "Alice Smith"))))
				(field (field "age")
					(e-int @4.49-4.51 (raw "32")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.7 (ident "person"))
		(e-record @1.10-1.52
			(fields
				(field (name "name")
					(e-string @1.18-1.25
						(e-literal @1.19-1.24 (string "Alice"))))
				(field (name "age")
					(e-int @1.32-1.34 (value "30")))
				(field (name "city")
					(e-string @1.42-1.50
						(e-literal @1.43-1.49 (string "Boston")))))))
	(d-let
		(p-assign @2.1-2.12 (ident "updated_one"))
		(e-record @2.15-2.36
			(ext
				(e-lookup-local @2.19-2.25
					(p-assign @1.1-1.7 (ident "person"))))
			(fields
				(field (name "age")
					(e-int @2.32-2.34 (value "31"))))))
	(d-let
		(p-assign @3.1-3.9 (ident "updated2"))
		(e-record @3.12-3.47
			(ext
				(e-lookup-local @3.16-3.27
					(p-assign @2.1-2.12 (ident "updated_one"))))
			(fields
				(field (name "city")
					(e-string @3.35-3.45
						(e-literal @3.36-3.44 (string "New York")))))))
	(d-let
		(p-assign @4.1-4.6 (ident "final"))
		(e-record @4.9-4.53
			(ext
				(e-lookup-local @4.13-4.21
					(p-assign @3.1-3.9 (ident "updated2"))))
			(fields
				(field (name "name")
					(e-string @4.29-4.42
						(e-literal @4.30-4.41 (string "Alice Smith"))))
				(field (name "age")
					(e-int @4.49-4.51 (value "32")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.7 (type "{ name: Str, age: Num(_size), city: Str }"))
		(patt @2.1-2.12 (type "{ age: Num(_size) }"))
		(patt @3.1-3.9 (type "{ city: Str }"))
		(patt @4.1-4.6 (type "{ name: Str, age: Num(_size) }")))
	(expressions
		(expr @1.10-1.52 (type "{ name: Str, age: Num(_size), city: Str }"))
		(expr @2.15-2.36 (type "{ age: Num(_size) }"))
		(expr @3.12-3.47 (type "{ city: Str }"))
		(expr @4.9-4.53 (type "{ name: Str, age: Num(_size) }"))))
~~~
