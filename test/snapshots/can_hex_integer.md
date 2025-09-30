# META
~~~ini
description=Hexadecimal integer literal type inference
type=file
~~~
# SOURCE
~~~roc
x = 0xFF
~~~
# EXPECTED
MISSING MAIN! FUNCTION - can_hex_integer.md:1:1:1:9
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**can_hex_integer.md:1:1:1:9:**
```roc
x = 0xFF
```
^^^^^^^^


# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Int(1:5-1:9),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.9
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.9
			(p-ident @1.1-1.2 (raw "x"))
			(e-int @1.5-1.9 (raw "0xFF")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "x"))
		(e-int @1.5-1.9 (value "255"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Int(_size)")))
	(expressions
		(expr @1.5-1.9 (type "Int(_size)"))))
~~~
