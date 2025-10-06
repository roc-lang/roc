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
LowerIdent(1:1-1:7),OpAssign(1:8-1:9),OpBar(1:10-1:11),LowerIdent(1:11-1:12),OpBar(1:12-1:13),LowerIdent(1:14-1:15),OpPlus(1:16-1:17),Int(1:18-1:19),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.19
	(type-module @1.1-1.7)
	(statements
		(s-decl @1.1-1.19
			(p-ident @1.1-1.7 (raw "helper"))
			(e-lambda @1.10-1.19
				(args
					(p-ident @1.11-1.12 (raw "x")))
				(e-binop @1.14-1.19 (op "+")
					(e-ident @1.14-1.15 (raw "x"))
					(e-int @1.18-1.19 (raw "1")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.7 (ident "helper"))
		(e-lambda @1.10-1.19
			(args
				(p-assign @1.11-1.12 (ident "x")))
			(e-binop @1.14-1.19 (op "add")
				(e-lookup-local @1.14-1.15
					(p-assign @1.11-1.12 (ident "x")))
				(e-num @1.18-1.19 (value "1"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.7 (type "Num(_size) -> Num(_size2)")))
	(expressions
		(expr @1.10-1.19 (type "Num(_size) -> Num(_size2)"))))
~~~
