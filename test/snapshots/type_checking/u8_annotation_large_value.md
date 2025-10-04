# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = 500
~~~
# EXPECTED
MODULE HEADER DEPRECATED - u8_annotation_large_value.md:1:1:1:10
NUMBER DOES NOT FIT IN TYPE - u8_annotation_large_value.md:4:5:4:8
# PROBLEMS
**MODULE HEADER DEPRECATED**
The `module` header is deprecated.

Type modules (headerless files with a top-level type matching the filename) are now the preferred way to define modules.

Remove the `module` header and ensure your file defines a type that matches the filename.
**u8_annotation_large_value.md:1:1:1:10:**
```roc
module []
```
^^^^^^^^^


**NUMBER DOES NOT FIT IN TYPE**
The number **500** does not fit in its inferred type:
**u8_annotation_large_value.md:4:5:4:8:**
```roc
x = 500
```
    ^^^

Its inferred type is:
    _Num(Int(Unsigned8))_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:8),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.8
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @3.1-3.7 (name "x")
			(ty @3.5-3.7 (name "U8")))
		(s-decl @4.1-4.8
			(p-ident @4.1-4.2 (raw "x"))
			(e-int @4.5-4.8 (raw "500")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "x"))
		(e-num @4.5-4.8 (value "500"))
		(annotation @4.1-4.2
			(declared-type
				(ty-lookup @3.5-3.7 (name "U8") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Error")))
	(expressions
		(expr @4.5-4.8 (type "Error"))))
~~~
