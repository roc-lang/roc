# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file:U8AnnotationLargeValue.roc
~~~
# SOURCE
~~~roc
U8AnnotationLargeValue := {}

x : U8
x = 500
~~~
# EXPECTED
NUMBER DOES NOT FIT IN TYPE - u8_annotation_large_value.md:4:5:4:8
# PROBLEMS
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
UpperIdent(1:1-1:23),OpColonEqual(1:24-1:26),OpenCurly(1:27-1:28),CloseCurly(1:28-1:29),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:8),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.8
	(type-module @1.1-1.23)
	(statements
		(s-type-decl @1.1-1.29
			(header @1.1-1.23 (name "U8AnnotationLargeValue")
				(args))
			(ty-record @1.27-1.29))
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
				(ty-lookup @3.5-3.7 (name "U8") (builtin)))))
	(s-nominal-decl @1.1-1.29
		(ty-header @1.1-1.23 (name "U8AnnotationLargeValue"))
		(ty-record @1.27-1.29)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Error")))
	(type_decls
		(nominal @1.1-1.29 (type "U8AnnotationLargeValue")
			(ty-header @1.1-1.23 (name "U8AnnotationLargeValue"))))
	(expressions
		(expr @4.5-4.8 (type "Error"))))
~~~
