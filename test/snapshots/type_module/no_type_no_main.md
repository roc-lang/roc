# META
~~~ini
description=Headerless module with no type and no main! reports invalid type module
type=file
~~~
# SOURCE
~~~roc
x = 5
~~~
# EXPECTED
MISSING MAIN! FUNCTION - no_type_no_main.md:1:1:1:6
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**no_type_no_main.md:1:1:1:6:**
```roc
x = 5
```
^^^^^


# TOKENS
~~~zig
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "5"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)")))
	(expressions
		(expr (type "Num(_size)"))))
~~~
