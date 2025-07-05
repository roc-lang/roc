# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = if tru then 0
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `no_else`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**expr_if_missing_else.md:3:19:3:20:**
```roc
foo = if tru then 0
```
                  ^


**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),KwIf(3:7-3:9),LowerIdent(3:10-3:13),LowerIdent(3:14-3:18),Int(3:19-3:20),EndOfFile(3:20-3:20),
~~~
# PARSE
~~~clojure
(file @1.1-3.20
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.20
			(p-ident @3.1-3.4 (raw "foo"))
			(e-malformed @3.19-3.20 (reason "no_else")))))
~~~
# FORMATTED
~~~roc
module []

foo = 
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.4 (ident "foo"))
		(e-runtime-error (tag "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.4 (type "Error")))
	(expressions
		(expr @3.19-3.20 (type "Error"))))
~~~
