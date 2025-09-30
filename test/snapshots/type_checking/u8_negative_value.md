# META
~~~ini
description=U8 type annotation with negative value
type=file
~~~
# SOURCE
~~~roc
x : U8
x = -1
~~~
# EXPECTED
MISSING MAIN! FUNCTION - u8_negative_value.md:1:1:2:7
NEGATIVE UNSIGNED INTEGER - u8_negative_value.md:2:5:2:7
# PROBLEMS
**MISSING MAIN! FUNCTION**
Default app modules must have a `main!` function.

No `main!` function was found.

Add a main! function like:
`main! = |arg| { ... }`
**u8_negative_value.md:1:1:2:7:**
```roc
x : U8
x = -1
```


**NEGATIVE UNSIGNED INTEGER**
The number **-1** is **signed** because it is negative:
**u8_negative_value.md:2:5:2:7:**
```roc
x = -1
```
    ^^

However, its inferred type is **unsigned**:
    _U8_

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:7),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:7),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.7
	(type-module @1.1-1.2)
	(statements
		(s-type-anno @1.1-1.7 (name "x")
			(ty @1.5-1.7 (name "U8")))
		(s-decl @2.1-2.7
			(p-ident @2.1-2.2 (raw "x"))
			(e-int @2.5-2.7 (raw "-1")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.2 (ident "x"))
		(e-int @2.5-2.7 (value "-1"))
		(annotation @2.1-2.2
			(declared-type
				(ty @1.5-1.7 (name "U8"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "Error")))
	(expressions
		(expr @2.5-2.7 (type "Error"))))
~~~
