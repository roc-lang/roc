# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = asd.0
~~~
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expr_no_space_dot_int`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**expr_no_space_dot_int.md:3:10:3:12:**
```roc
foo = asd.0
```
         ^^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),LowerIdent(3:7-3:10),NoSpaceDotInt(3:10-3:12),EndOfFile(3:12-3:12),
~~~
# PARSE
~~~clojure
(file @1-1-3-12
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-decl @3-1-3-12
			(p-ident @3-1-3-4 (raw "foo"))
			(e-malformed @3-10-3-12 (reason "expr_no_space_dot_int")))))
~~~
# FORMATTED
~~~roc
module []

foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 75)
		(p-assign @3-1-3-4 (ident "foo") (id 72))
		(e-runtime-error (tag "expr_not_canonicalized") (id 74))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "foo") (def_var 75) (type "Error")))
	(expressions
		(expr @3-10-3-12 (type "Error"))))
~~~
