# META
~~~ini
description=U8 type annotation with negative value
type=snippet
~~~
# SOURCE
~~~roc
x : U8
x = -1
~~~
# EXPECTED
TYPE MISMATCH - u8_negative_value.md:2:5:2:7
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**u8_negative_value.md:2:5:2:7:**
```roc
x = -1
```
    ^^

It has the type:
    _Num(_size)_

But the type annotation says it should have the type:
    _U8_

# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "x")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "-1")))))
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
		(e-num (value "-1"))
		(annotation
			(ty-lookup (name "U8") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
