# META
~~~ini
description=Basic record updater with field override
type=file
~~~
# SOURCE
~~~roc
module [person, updated]

person = { name: "Alice", age: 30 }
updated = { ..person, age: 31 }
~~~
# EXPECTED
MODULE HEADER DEPRECATED - record_updater_basic.md:1:1:1:25
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**record_updater_basic.md:1:1:1:25:**
```roc
module [person, updated]
```
^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:15),Comma(1:15-1:16),LowerIdent(1:17-1:24),CloseSquare(1:24-1:25),
LowerIdent(3:1-3:7),OpAssign(3:8-3:9),OpenCurly(3:10-3:11),LowerIdent(3:12-3:16),OpColon(3:16-3:17),StringStart(3:18-3:19),StringPart(3:19-3:24),StringEnd(3:24-3:25),Comma(3:25-3:26),LowerIdent(3:27-3:30),OpColon(3:30-3:31),Int(3:32-3:34),CloseCurly(3:35-3:36),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpenCurly(4:11-4:12),DoubleDot(4:13-4:15),LowerIdent(4:15-4:21),Comma(4:21-4:22),LowerIdent(4:23-4:26),OpColon(4:26-4:27),Int(4:28-4:30),CloseCurly(4:31-4:32),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.32
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
		(s-decl @4.1-4.32
			(p-ident @4.1-4.8 (raw "updated"))
			(e-record @4.11-4.32
				(ext
					(e-ident @4.15-4.21 (raw "person")))
				(field (field "age")
					(e-int @4.28-4.30 (raw "31")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.7 (ident "person"))
		(e-record @3.10-3.36
			(fields
				(field (name "name")
					(e-string @3.18-3.25
						(e-literal @3.19-3.24 (string "Alice"))))
				(field (name "age")
					(e-num @3.32-3.34 (value "30"))))))
	(d-let
		(p-assign @4.1-4.8 (ident "updated"))
		(e-record @4.11-4.32
			(ext
				(e-lookup-local @4.15-4.21
					(p-assign @3.1-3.7 (ident "person"))))
			(fields
				(field (name "age")
					(e-num @4.28-4.30 (value "31")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.7 (type "{ age: Num(_size), name: Str }"))
		(patt @4.1-4.8 (type "{ age: Num(_size), { age: Num(_size2), name: Str } }")))
	(expressions
		(expr @3.10-3.36 (type "{ age: Num(_size), name: Str }"))
		(expr @4.11-4.32 (type "{ age: Num(_size), { age: Num(_size2), name: Str } }"))))
~~~
