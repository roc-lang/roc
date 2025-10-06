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
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:7),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Int(2:5-2:8),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.8
	(type-module @1.1-1.2)
	(statements
		(s-type-anno @1.1-1.7 (name "x")
			(ty @1.5-1.7 (name "U8")))
		(s-decl @2.1-2.8
			(p-ident @2.1-2.2 (raw "x"))
			(e-int @2.5-2.8 (raw "500")))))
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
		(e-num @2.5-2.8 (value "500"))
		(annotation @2.1-2.2
			(declared-type
				(ty-lookup @1.5-1.7 (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "Error")))
	(expressions
		(expr @2.5-2.8 (type "Error"))))
~~~
