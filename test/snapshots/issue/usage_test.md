# META
~~~ini
description=Test if usage affects error type conversion
type=snippet
~~~
# SOURCE
~~~roc
UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
TYPE MISMATCH - usage_test.md:6:9:6:11
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**usage_test.md:1:1:1:1:**
```roc
UnusedType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**usage_test.md:1:1:1:1:**
```roc
UnusedType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**usage_test.md:6:9:6:11:**
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
UpperIdent(1:1-1:11),OpColonEqual(1:12-1:14),Underscore(1:15-1:16),
UpperIdent(3:1-3:9),OpColonEqual(3:10-3:12),Underscore(3:13-3:14),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:17),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),Int(6:9-6:11),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.11
	(type-module @1.1-1.11)
	(statements
		(s-type-decl @1.1-1.16
			(header @1.1-1.11 (name "UnusedType")
				(args))
			(_))
		(s-type-decl @3.1-3.14
			(header @3.1-3.9 (name "UsedType")
				(args))
			(_))
		(s-type-anno @5.1-5.17 (name "value")
			(ty @5.9-5.17 (name "UsedType")))
		(s-decl @6.1-6.11
			(p-ident @6.1-6.6 (raw "value"))
			(e-int @6.9-6.11 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.6 (ident "value"))
		(e-num @6.9-6.11 (value "42"))
		(annotation @6.1-6.6
			(declared-type
				(ty-lookup @5.9-5.17 (name "UsedType") (local)))))
	(s-nominal-decl @1.1-1.16
		(ty-header @1.1-1.11 (name "UnusedType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @3.1-3.14
		(ty-header @3.1-3.9 (name "UsedType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "Error")))
	(type_decls
		(nominal @1.1-1.16 (type "UnusedType")
			(ty-header @1.1-1.11 (name "UnusedType")))
		(nominal @3.1-3.14 (type "UsedType")
			(ty-header @3.1-3.9 (name "UsedType"))))
	(expressions
		(expr @6.9-6.11 (type "Error"))))
~~~
