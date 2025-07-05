# META
~~~ini
description=Record in let binding statement
type=statement
~~~
# SOURCE
~~~roc
person = { name: "Alice", age: 30, email: "alice@example.com" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:7),OpAssign(1:8-1:9),OpenCurly(1:10-1:11),LowerIdent(1:12-1:16),OpColon(1:16-1:17),StringStart(1:18-1:19),StringPart(1:19-1:24),StringEnd(1:24-1:25),Comma(1:25-1:26),LowerIdent(1:27-1:30),OpColon(1:30-1:31),Int(1:32-1:34),Comma(1:34-1:35),LowerIdent(1:36-1:41),OpColon(1:41-1:42),StringStart(1:43-1:44),StringPart(1:44-1:61),StringEnd(1:61-1:62),CloseCurly(1:63-1:64),EndOfFile(1:64-1:64),
~~~
# PARSE
~~~clojure
(s-decl @1.1-1.64
	(p-ident @1.1-1.7 (raw "person"))
	(e-record @1.10-1.64
		(field (field "name") (optional false)
			(e-string @1.18-1.25
				(e-string-part @1.19-1.24 (raw "Alice"))))
		(field (field "age") (optional false)
			(e-int @1.32-1.34 (raw "30")))
		(field (field "email") (optional false)
			(e-string @1.43-1.62
				(e-string-part @1.44-1.61 (raw "alice@example.com"))))))
~~~
# FORMATTED
~~~roc
person = {name: "Alice", age: 30, email: "alice@example.com"}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-let @1.1-1.64
		(p-assign @1.1-1.7 (ident "person"))
		(e-record @1.10-1.64
			(fields
				(field (name "name")
					(e-string @1.18-1.25
						(e-literal @1.19-1.24 (string "Alice"))))
				(field (name "age")
					(e-int @1.32-1.34 (value "30")))
				(field (name "email")
					(e-string @1.43-1.62
						(e-literal @1.44-1.61 (string "alice@example.com"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
