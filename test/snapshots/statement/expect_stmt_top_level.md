# META
~~~ini
description=Debug expression stmt
type=snippet
~~~
# SOURCE
~~~roc
foo = Bool.True

expect foo != Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**expect_stmt_top_level.md:1:7:1:11:**
```roc
foo = Bool.True
```
      ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**expect_stmt_top_level.md:3:15:3:19:**
```roc
expect foo != Bool.False
```
              ^^^^


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
		(e-runtime-error (tag "undeclared_type")))
	(s-expect @3.1-3.25
		(e-binop @3.8-3.25 (op "ne")
			(e-lookup-local @3.8-3.11
				(p-assign @1.1-1.4 (ident "foo")))
			(e-runtime-error (tag "undeclared_type")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Error")))
	(expressions
		(expr @1.7-1.11 (type "Error"))))
~~~
