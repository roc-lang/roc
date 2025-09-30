# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
foo = asd.0
~~~
# EXPECTED
PARSE ERROR - expr_no_space_dot_int.md:1:10:1:12
MISSING MAIN! FUNCTION - expr_no_space_dot_int.md:1:1:1:12
UNRECOGNIZED SYNTAX - expr_no_space_dot_int.md:1:10:1:12
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

**expr_no_space_dot_int.md:1:10:1:12:**
```roc
foo = asd.0
```
         ^^


**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**expr_no_space_dot_int.md:1:1:1:12:**
```roc
foo = asd.0
```
^^^^^^^^^^^


**UNRECOGNIZED SYNTAX**
I don't recognize this syntax.

**expr_no_space_dot_int.md:1:10:1:12:**
```roc
foo = asd.0
```
         ^^

This might be a syntax error, an unsupported language feature, or a typo.

# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),LowerIdent(1:7-1:10),NoSpaceDotInt(1:10-1:12),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.12
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.12
			(p-ident @1.1-1.4 (raw "foo"))
			(e-malformed @1.10-1.12 (reason "expr_no_space_dot_int")))))
~~~
# FORMATTED
~~~roc
foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Error")))
	(expressions
		(expr @1.10-1.12 (type "Error"))))
~~~
