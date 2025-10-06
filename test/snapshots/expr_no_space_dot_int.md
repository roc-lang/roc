# META
~~~ini
description=
type=file:ExprNoSpaceDotInt.roc
~~~
# SOURCE
~~~roc
ExprNoSpaceDotInt := {}

foo = asd.0
~~~
# EXPECTED
PARSE ERROR - expr_no_space_dot_int.md:3:10:3:12
UNRECOGNIZED SYNTAX - expr_no_space_dot_int.md:3:10:3:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

**expr_no_space_dot_int.md:3:10:3:12:**
```roc
foo = asd.0
```
         ^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**expr_no_space_dot_int.md:3:10:3:12:**
```roc
foo = asd.0
```
         ^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
UpperIdent(1:1-1:18),OpColonEqual(1:19-1:21),OpenCurly(1:22-1:23),CloseCurly(1:23-1:24),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),LowerIdent(3:7-3:10),NoSpaceDotInt(3:10-3:12),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.12
	(type-module @1.1-1.18)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.18 (name "ExprNoSpaceDotInt")
				(args))
			(ty-record @1.22-1.24))
		(s-decl @3.1-3.12
			(p-ident @3.1-3.4 (raw "foo"))
			(e-malformed @3.10-3.12 (reason "expr_no_space_dot_int")))))
~~~
# FORMATTED
~~~roc
ExprNoSpaceDotInt := {}

foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized")))
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.18 (name "ExprNoSpaceDotInt"))
		(ty-record @1.22-1.24)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error")))
	(type_decls
		(nominal @1.1-1.24 (type "ExprNoSpaceDotInt")
			(ty-header @1.1-1.18 (name "ExprNoSpaceDotInt"))))
	(expressions
		(expr @3.10-3.12 (type "Error"))))
~~~
