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
UNDECLARED TYPE - expect_stmt_top_level.md:1:7:1:11
UNDECLARED TYPE - expect_stmt_top_level.md:3:15:3:19
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
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
KwExpect,LowerIdent,OpNotEquals,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-tag (raw "Bool.True")))
		(s-expect
			(e-binop (op "!=")
				(e-ident (raw "foo"))
				(e-tag (raw "Bool.False"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "undeclared_type")))
	(s-expect
		(e-binop (op "ne")
			(e-lookup-local
				(p-assign (ident "foo")))
			(e-runtime-error (tag "undeclared_type")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
