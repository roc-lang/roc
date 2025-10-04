# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
module [foo]

foo = Bool.True

expect foo != Bool.False
~~~
# EXPECTED
MODULE HEADER DEPRECATED - expect_stmt_top_level.md:1:1:1:13
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**expect_stmt_top_level.md:1:1:1:13:**
```roc
module [foo]
```
^^^^^^^^^^^^


# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),UpperIdent(3:7-3:11),NoSpaceDotUpperIdent(3:11-3:16),
KwExpect(5:1-5:7),LowerIdent(5:8-5:11),OpNotEquals(5:12-5:14),UpperIdent(5:15-5:19),NoSpaceDotUpperIdent(5:19-5:25),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.25
	(module @1.1-1.13
		(exposes @1.8-1.13
			(exposed-lower-ident @1.9-1.12
				(text "foo"))))
	(statements
		(s-decl @3.1-3.16
			(p-ident @3.1-3.4 (raw "foo"))
			(e-tag @3.7-3.16 (raw "Bool.True")))
		(s-expect @5.1-5.25
			(e-binop @5.8-5.25 (op "!=")
				(e-ident @5.8-5.11 (raw "foo"))
				(e-tag @5.15-5.25 (raw "Bool.False"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-nominal @3.7-3.16 (nominal "Bool")
			(e-tag @3.7-3.16 (name "True"))))
	(s-expect @5.1-5.25
		(e-binop @5.8-5.25 (op "ne")
			(e-lookup-local @5.8-5.11
				(p-assign @3.1-3.4 (ident "foo")))
			(e-nominal @5.15-5.25 (nominal "Bool")
				(e-tag @5.15-5.25 (name "False"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Bool")))
	(expressions
		(expr @3.7-3.16 (type "Bool"))))
~~~
