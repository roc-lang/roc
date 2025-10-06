# META
~~~ini
description=Error types should propagate through aliases when underscores are used
type=snippet
~~~
# SOURCE
~~~roc
BadBase := _

BadDerived := BadBase

value : BadDerived
value = "test"

GoodBase := Str

GoodDerived := GoodBase

goodValue : GoodDerived
goodValue = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - underscore_error_propagation.md:1:1:1:1
TYPE MISMATCH - underscore_error_propagation.md:6:9:6:15
TYPE MISMATCH - underscore_error_propagation.md:13:13:13:19
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_propagation.md:1:1:1:1:**
```roc
BadBase := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_propagation.md:6:9:6:15:**
```roc
value = "test"
```
        ^^^^^^

It has the type:
    _Str_

But the type annotation says it should have the type:
    _BadDerived_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_propagation.md:13:13:13:19:**
```roc
goodValue = "test"
```
            ^^^^^^

It has the type:
    _Str_

But the type annotation says it should have the type:
    _GoodDerived_

# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),Underscore(1:12-1:13),
UpperIdent(3:1-3:11),OpColonEqual(3:12-3:14),UpperIdent(3:15-3:22),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:19),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),StringStart(6:9-6:10),StringPart(6:10-6:14),StringEnd(6:14-6:15),
UpperIdent(8:1-8:9),OpColonEqual(8:10-8:12),UpperIdent(8:13-8:16),
UpperIdent(10:1-10:12),OpColonEqual(10:13-10:15),UpperIdent(10:16-10:24),
LowerIdent(12:1-12:10),OpColon(12:11-12:12),UpperIdent(12:13-12:24),
LowerIdent(13:1-13:10),OpAssign(13:11-13:12),StringStart(13:13-13:14),StringPart(13:14-13:18),StringEnd(13:18-13:19),
EndOfFile(14:1-14:1),
~~~
# PARSE
~~~clojure
(file @1.1-13.19
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.8 (name "BadBase")
				(args))
			(_))
		(s-type-decl @3.1-3.22
			(header @3.1-3.11 (name "BadDerived")
				(args))
			(ty @3.15-3.22 (name "BadBase")))
		(s-type-anno @5.1-5.19 (name "value")
			(ty @5.9-5.19 (name "BadDerived")))
		(s-decl @6.1-6.15
			(p-ident @6.1-6.6 (raw "value"))
			(e-string @6.9-6.15
				(e-string-part @6.10-6.14 (raw "test"))))
		(s-type-decl @8.1-8.16
			(header @8.1-8.9 (name "GoodBase")
				(args))
			(ty @8.13-8.16 (name "Str")))
		(s-type-decl @10.1-10.24
			(header @10.1-10.12 (name "GoodDerived")
				(args))
			(ty @10.16-10.24 (name "GoodBase")))
		(s-type-anno @12.1-12.24 (name "goodValue")
			(ty @12.13-12.24 (name "GoodDerived")))
		(s-decl @13.1-13.19
			(p-ident @13.1-13.10 (raw "goodValue"))
			(e-string @13.13-13.19
				(e-string-part @13.14-13.18 (raw "test"))))))
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
		(e-string @6.9-6.15
			(e-literal @6.10-6.14 (string "test")))
		(annotation @6.1-6.6
			(declared-type
				(ty-lookup @5.9-5.19 (name "BadDerived") (local)))))
	(d-let
		(p-assign @13.1-13.10 (ident "goodValue"))
		(e-string @13.13-13.19
			(e-literal @13.14-13.18 (string "test")))
		(annotation @13.1-13.10
			(declared-type
				(ty-lookup @12.13-12.24 (name "GoodDerived") (local)))))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.8 (name "BadBase"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @3.1-3.22
		(ty-header @3.1-3.11 (name "BadDerived"))
		(ty-lookup @3.15-3.22 (name "BadBase") (local)))
	(s-nominal-decl @8.1-8.16
		(ty-header @8.1-8.9 (name "GoodBase"))
		(ty-lookup @8.13-8.16 (name "Str") (builtin)))
	(s-nominal-decl @10.1-10.24
		(ty-header @10.1-10.12 (name "GoodDerived"))
		(ty-lookup @10.16-10.24 (name "GoodBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "Error"))
		(patt @13.1-13.10 (type "Error")))
	(type_decls
		(nominal @1.1-1.13 (type "BadBase")
			(ty-header @1.1-1.8 (name "BadBase")))
		(nominal @3.1-3.22 (type "BadDerived")
			(ty-header @3.1-3.11 (name "BadDerived")))
		(nominal @8.1-8.16 (type "GoodBase")
			(ty-header @8.1-8.9 (name "GoodBase")))
		(nominal @10.1-10.24 (type "GoodDerived")
			(ty-header @10.1-10.12 (name "GoodDerived"))))
	(expressions
		(expr @6.9-6.15 (type "Error"))
		(expr @13.13-13.19 (type "Error"))))
~~~
