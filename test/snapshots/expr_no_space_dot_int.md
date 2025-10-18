# META
~~~ini
description=
type=snippet
~~~
# SOURCE
~~~roc
foo = asd.0
~~~
# EXPECTED
PARSE ERROR - expr_no_space_dot_int.md:1:10:1:12
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
LowerIdent,OpAssign,LowerIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "foo"))
			(e-malformed (reason "expr_no_space_dot_int")))))
~~~
# FORMATTED
~~~roc
foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
