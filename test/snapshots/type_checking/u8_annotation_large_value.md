# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=snippet
~~~
# SOURCE
~~~roc
x : U8
x = 500
~~~
# EXPECTED
INVALID NUMERIC LITERAL - u8_annotation_large_value.md:2:5:2:8
# PROBLEMS
**INVALID NUMERIC LITERAL**
This numeric literal cannot be represented as the expected type:
**u8_annotation_large_value.md:2:5:2:8:**
```roc
x = 500
```
    ^^^

The expected type is:

    U8

The value is outside the valid range for that type.

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
			(e-int (raw "500")))))
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
		(e-num (value "500"))
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
