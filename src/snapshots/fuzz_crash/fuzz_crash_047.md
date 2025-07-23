# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person,
 age: 31 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:24),CloseSquare(1:24-1:25),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),OpenCurly(3:10-3:11),LowerIdent(3:12-3:16),OpColon(3:16-3:17),StringStart(3:18-3:19),StringPart(3:19-3:24),StringEnd(3:24-3:25),Comma(3:25-3:26),LowerIdent(3:27-3:30),OpColon(3:30-3:31),Int(3:32-3:34),CloseCurly(3:35-3:36),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpenCurly(4:11-4:12),DoubleDot(4:13-4:15),LowerIdent(4:15-4:21),Comma(4:21-4:22),
LowerIdent(5:2-5:5),OpColon(5:5-5:6),Int(5:7-5:9),CloseCurly(5:10-5:11),EndOfFile(5:11-5:11),
~~~
# PARSE
~~~clojure
(file @1.1-5.11
	(module @1.1-1.25
		(exposes @1.8-1.25
			(exposed-lower-ident @1.9-1.15
				(text "person"))
			(exposed-lower-ident @1.17-1.24
				(text "updated"))))
	(statements
		(s-decl @3.1-3.36
			(p-ident @3.1-3.7 (raw "person"))
			(e-record @3.10-3.36
				(field (field "name")
					(e-string @3.18-3.25
						(e-string-part @3.19-3.24 (raw "Alice"))))
				(field (field "age")
					(e-int @3.32-3.34 (raw "30")))))
		(s-decl @4.1-5.11
			(p-ident @4.1-4.8 (raw "updated"))
			(e-record @4.11-5.11
				(ext
					(e-ident @4.15-4.21 (raw "person")))
				(field (field "age")
					(e-int @5.7-5.9 (raw "31")))))))
~~~
# FORMATTED
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = {
	..person,
	age: 31,
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @3.1-3.7 (ident "person")))
		(expr
			(e-record @3.10-3.36
				(fields
					(record-field (label "name")
						(value
							(e-string @3.18-3.25
								(e-literal @3.19-3.24 (string "Alice")))))
					(record-field (label "age")
						(value
							(e-int @3.32-3.34 (value "30"))))))))
	(def
		(pattern
			(p-assign @4.1-4.8 (ident "updated")))
		(expr
			(e-record @4.11-5.11
				(ext
					(e-lookup-local @4.15-4.21
						(p-assign @3.1-3.7 (ident "person"))))
				(fields
					(record-field (label "age")
						(value
							(e-int @5.7-5.9 (value "31")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "{ name: Str, age: Num(_size) }"))
		(patt @4.1-4.8 (type "{ age: Num(_size) }")))
	(expressions
		(expr @3.10-3.36 (type "{ name: Str, age: Num(_size) }"))
		(expr @4.11-5.11 (type "{ age: Num(_size) }"))))
~~~
