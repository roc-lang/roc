# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
foo = 42
~~~
# EXPECTED
MISSING MAIN! FUNCTION - expr_int.md:1:1:1:9
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**expr_int.md:1:1:1:9:**
```roc
foo = 42
```
^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:4),OpAssign(1:5-1:6),Int(1:7-1:9),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.9
	(type-module @1.1-1.4)
	(statements
		(s-decl @1.1-1.9
			(p-ident @1.1-1.4 (raw "foo"))
			(e-int @1.7-1.9 (raw "42")))))
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
		(e-int @1.7-1.9 (value "42"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.4 (type "Num(_size)")))
	(expressions
		(expr @1.7-1.9 (type "Num(_size)"))))
~~~
