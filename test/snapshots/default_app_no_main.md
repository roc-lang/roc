# META
~~~ini
description=Error - type module with no main! or matching type
type=file
~~~
# SOURCE
~~~roc
helper = |x| x + 1
~~~
# EXPECTED
MISSING MAIN! FUNCTION - default_app_no_main.md:1:1:1:19
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**default_app_no_main.md:1:1:1:19:**
```roc
helper = |x| x + 1
```
^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "helper"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-int (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "helper"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-num (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size) -> Num(_size2)")))
	(expressions
		(expr (type "Num(_size) -> Num(_size2)"))))
~~~
