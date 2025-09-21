# META
~~~ini
description=Test if usage affects error type conversion
type=file
~~~
# SOURCE
~~~roc
module []

UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
TYPE MISMATCH - usage_test.md:8:9:8:11
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**usage_test.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**usage_test.md:1:1:1:1:**
```roc
module []
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**usage_test.md:8:9:8:11:**
```roc
value = 42
```
        ^^

It has the type:
    _Num(_size)_

But the type annotation says it should have the type:
    _UsedType_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:11),OpColonEqual(3:12-3:14),Underscore(3:15-3:16),
UpperIdent(5:1-5:9),OpColonEqual(5:10-5:12),Underscore(5:13-5:14),
LowerIdent(7:1-7:6),OpColon(7:7-7:8),UpperIdent(7:9-7:17),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),Int(8:9-8:11),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.11
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.16
			(header @3.1-3.11 (name "UnusedType")
				(args))
			(_))
		(s-type-decl @5.1-5.14
			(header @5.1-5.9 (name "UsedType")
				(args))
			(_))
		(s-type-anno @7.1-7.17 (name "value")
			(ty @7.9-7.17 (name "UsedType")))
		(s-decl @8.1-8.11
			(p-ident @8.1-8.6 (raw "value"))
			(e-int @8.9-8.11 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @8.1-8.6 (ident "value"))
		(e-num @8.9-8.11 (value "42"))
		(annotation @8.1-8.6
			(declared-type
				(ty-lookup @7.9-7.17 (name "UsedType") (local)))))
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
			(tag_name @1.1-1.1 (name "Err"))))
	(s-nominal-decl @3.1-3.16
		(ty-header @3.1-3.11 (name "UnusedType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @5.1-5.14
		(ty-header @5.1-5.9 (name "UsedType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.6 (type "Error")))
	(type_decls
		(nominal @1.1-1.1 (type "Bool")
			(ty-header @1.1-1.1 (name "Bool")))
		(nominal @1.1-1.1 (type "Result(ok, err)")
			(ty-header @1.1-1.1 (name "Result")
				(ty-args
					(ty-rigid-var @1.1-1.1 (name "ok"))
					(ty-rigid-var @1.1-1.1 (name "err")))))
		(nominal @3.1-3.16 (type "UnusedType")
			(ty-header @3.1-3.11 (name "UnusedType")))
		(nominal @5.1-5.14 (type "UsedType")
			(ty-header @5.1-5.9 (name "UsedType"))))
	(expressions
		(expr @8.9-8.11 (type "Error"))))
~~~
