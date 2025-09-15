# META
~~~ini
description=U8 type annotation with negative value
type=file
~~~
# SOURCE
~~~roc
module []

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
    _U8_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:7),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.7
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
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
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Bool"))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "True"))
			(tag_name @1.1-1.1 (name "False"))))
	(s-nominal-decl @1.1-1.1
		(ty-header @1.1-1.1 (name "Result")
			(ty-args
				(ty-rigid-var @1.1-1.1 (name "ok"))
				(ty-rigid-var @1.1-1.1 (name "err"))))
		(ty-tag-union @1.1-1.1
			(tag_name @1.1-1.1 (name "Ok"))
			(tag_name @1.1-1.1 (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Error")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err"))))))
	(expressions
		(expr @4.5-4.7 (type "Error"))))
~~~
