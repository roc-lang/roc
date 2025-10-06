# META
~~~ini
description=U8 type annotation with negative value
type=file:U8NegativeValue.roc
~~~
# SOURCE
~~~roc
U8NegativeValue := {}

x : U8
x = -1
~~~
# EXPECTED
NEGATIVE UNSIGNED INTEGER - u8_negative_value.md:4:5:4:7
# PROBLEMS
**NEGATIVE UNSIGNED INTEGER**
The number **-1** is **signed** because it is negative:
**u8_negative_value.md:4:5:4:7:**
```roc
x = -1
```
    ^^

However, its inferred type is **unsigned**:
    _Num(Int(Unsigned8))_

# TOKENS
~~~zig
UpperIdent(1:1-1:16),OpColonEqual(1:17-1:19),OpenCurly(1:20-1:21),CloseCurly(1:21-1:22),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:7),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.7
	(type-module @1.1-1.16)
	(statements
		(s-type-decl @1.1-1.22
			(header @1.1-1.16 (name "U8NegativeValue")
				(args))
			(ty-record @1.20-1.22))
		(s-type-anno @3.1-3.7 (name "x")
			(ty @3.5-3.7 (name "U8")))
		(s-decl @4.1-4.7
			(p-ident @4.1-4.2 (raw "x"))
			(e-int @4.5-4.7 (raw "-1")))))
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
		(e-num @4.5-4.7 (value "-1"))
		(annotation @4.1-4.2
			(declared-type
				(ty-lookup @3.5-3.7 (name "U8") (builtin)))))
	(s-nominal-decl @1.1-1.22
		(ty-header @1.1-1.16 (name "U8NegativeValue"))
		(ty-record @1.20-1.22)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Error")))
	(type_decls
		(nominal @1.1-1.22 (type "U8NegativeValue")
			(ty-header @1.1-1.16 (name "U8NegativeValue"))))
	(expressions
		(expr @4.5-4.7 (type "Error"))))
~~~
