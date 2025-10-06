# META
~~~ini
description=fuzz crash
type=snippet
~~~
# SOURCE
~~~roc
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
LowerIdent(1:1-1:7),OpAssign(1:8-1:9),OpenCurly(1:10-1:11),LowerIdent(1:12-1:16),OpColon(1:16-1:17),StringStart(1:18-1:19),StringPart(1:19-1:24),StringEnd(1:24-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:30),OpColon(1:30-1:31),Int(1:32-1:34),CloseCurly(1:35-1:36),
LowerIdent(2:1-2:8),OpAssign(2:9-2:10),OpenCurly(2:11-2:12),DoubleDot(2:13-2:15),LowerIdent(2:15-2:21),Comma(2:21-2:22),
LowerIdent(3:2-3:5),OpColon(3:5-3:6),Int(3:7-3:9),CloseCurly(3:10-3:11),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.11
	(type-module @1.1-1.7)
	(statements
		(s-decl @1.1-1.36
			(p-ident @1.1-1.7 (raw "person"))
			(e-record @1.10-1.36
				(field (field "name")
					(e-string @1.18-1.25
						(e-string-part @1.19-1.24 (raw "Alice"))))
				(field (field "age")
					(e-int @1.32-1.34 (raw "30")))))
		(s-decl @2.1-3.11
			(p-ident @2.1-2.8 (raw "updated"))
			(e-record @2.11-3.11
				(ext
					(e-ident @2.15-2.21 (raw "person")))
				(field (field "age")
					(e-int @3.7-3.9 (raw "31")))))))
~~~
# FORMATTED
~~~roc
person = { name: "Alice", age: 30 }
updated = {
	..person,
	age: 31,
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.7 (ident "person"))
		(e-record @1.10-1.36
			(fields
				(field (name "name")
					(e-string @1.18-1.25
						(e-literal @1.19-1.24 (string "Alice"))))
				(field (name "age")
					(e-num @1.32-1.34 (value "30"))))))
	(d-let
		(p-assign @2.1-2.8 (ident "updated"))
		(e-record @2.11-3.11
			(ext
				(e-lookup-local @2.15-2.21
					(p-assign @1.1-1.7 (ident "person"))))
			(fields
				(field (name "age")
					(e-num @3.7-3.9 (value "31")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.7 (type "{ age: Num(_size), name: Str }"))
		(patt @2.1-2.8 (type "{ age: Num(_size), { age: Num(_size2), name: Str } }")))
	(expressions
		(expr @1.10-1.36 (type "{ age: Num(_size), name: Str }"))
		(expr @2.11-3.11 (type "{ age: Num(_size), { age: Num(_size2), name: Str } }"))))
~~~
