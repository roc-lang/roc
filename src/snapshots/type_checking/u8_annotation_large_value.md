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
# PROBLEMS
**NUMBER DOES NOT FIT IN TYPE**
The number **500** does not fit in its inferred type:
**u8_annotation_large_value.md:4:5:4:8:**
```roc
x = 500
```

Its inferred type is:
    _U8_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:8),EndOfFile(4:8-4:8),
~~~
# PARSE
~~~clojure
(file @1-1-4-8
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-type-anno @3-1-4-2 (name "x")
			(ty (name "U8")))
		(s-decl @4-1-4-8
			(p-ident @4-1-4-2 (raw "x"))
			(e-int @4-5-4-8 (raw "500")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 78)
		(p-assign @4-1-4-2 (ident "x") (id 73))
		(e-int @4-5-4-8 (value "500") (id 74))
		(annotation @4-1-4-2 (signature 76) (id 77)
			(declared-type
				(ty @3-5-3-7 (name "U8"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "x") (def_var 78) (type "Error")))
	(expressions
		(expr @4-5-4-8 (type "Error"))))
~~~
