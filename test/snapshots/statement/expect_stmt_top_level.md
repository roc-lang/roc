# META
~~~ini
description=Debug expression stmt
type=file
~~~
# SOURCE
~~~roc
foo = Bool.True

expect foo != Bool.False
~~~
# EXPECTED
MISSING MAIN! FUNCTION - expect_stmt_top_level.md:1:1:3:25
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**expect_stmt_top_level.md:1:1:3:25:**
```roc
foo = Bool.True

expect foo != Bool.False
```


# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),UpperIdent(1:7-1:11),NoSpaceDotUpperIdent(1:11-1:16),
KwExpect(3:1-3:7),LowerIdent(3:8-3:11),OpNotEquals(3:12-3:14),UpperIdent(3:15-3:19),NoSpaceDotUpperIdent(3:19-3:25),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.25
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.16
			(p-ident @1.1-1.4 (raw "foo"))
			(e-tag @1.7-1.16 (raw "Bool.True")))
		(s-expect @3.1-3.25
			(e-binop @3.8-3.25 (op "!=")
				(e-ident @3.8-3.11 (raw "foo"))
				(e-tag @3.15-3.25 (raw "Bool.False"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "foo"))
		(e-nominal @1.7-1.16 (nominal "Bool")
			(e-tag @1.7-1.16 (name "True"))))
	(s-expect @3.1-3.25
		(e-binop @3.8-3.25 (op "ne")
			(e-lookup-local @3.8-3.11
				(p-assign @1.1-1.4 (ident "foo")))
			(e-nominal @3.15-3.25 (nominal "Bool")
				(e-tag @3.15-3.25 (name "False"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Bool")))
	(expressions
		(expr @1.7-1.16 (type "Bool"))))
~~~
