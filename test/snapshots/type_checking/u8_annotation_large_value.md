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
TYPE MISMATCH - u8_annotation_large_value.md:2:5:2:8
TYPE MISMATCH - u8_annotation_large_value.md:2:5:2:8
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**u8_annotation_large_value.md:2:5:2:8:**
```roc
x = 500
```
    ^^^

It has the type:
    _Numeral_

But I expected it to be:
    _Num.Numeral_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**u8_annotation_large_value.md:2:5:2:8:**
```roc
x = 500
```
    ^^^

It has the type:
    _Try(U8, [InvalidNumeral(Str)])_

But I expected it to be:
    _Try(U8, [InvalidNumeral(Str)])_

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
