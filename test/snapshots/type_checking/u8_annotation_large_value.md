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
NUMBER DOES NOT FIT IN TYPE - u8_annotation_large_value.md:2:5:2:8
# PROBLEMS
**NUMBER DOES NOT FIT IN TYPE**
The number **500** does not fit in its inferred type:
**u8_annotation_large_value.md:2:5:2:8:**
```roc
x = 500
```
    ^^^

Its inferred type is:
    _Num(Int(Unsigned8))_

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
			(declared-type
				(ty-lookup (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
