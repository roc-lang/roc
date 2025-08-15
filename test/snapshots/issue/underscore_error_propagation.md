# META
~~~ini
description=Error types should propagate through aliases when underscores are used
type=file
~~~
# SOURCE
~~~roc
module []

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
TYPE MISMATCH - underscore_error_propagation.md:14:13:14:24
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_propagation.md:1:1:1:1:**
```roc
module []
```


Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**underscore_error_propagation.md:14:13:14:24:**
```roc
goodValue : GoodDerived
```
            ^^^^^^^^^^^

It is of type:
    _GoodDerived_

But the type annotation says it should have the type:
    _Str_

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:8),OpColonEqual(3:9-3:11),Underscore(3:12-3:13),
UpperIdent(5:1-5:11),OpColonEqual(5:12-5:14),UpperIdent(5:15-5:22),
LowerIdent(7:1-7:6),OpColon(7:7-7:8),UpperIdent(7:9-7:19),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),StringStart(8:9-8:10),StringPart(8:10-8:14),StringEnd(8:14-8:15),
UpperIdent(10:1-10:9),OpColonEqual(10:10-10:12),UpperIdent(10:13-10:16),
UpperIdent(12:1-12:12),OpColonEqual(12:13-12:15),UpperIdent(12:16-12:24),
LowerIdent(14:1-14:10),OpColon(14:11-14:12),UpperIdent(14:13-14:24),
LowerIdent(15:1-15:10),OpAssign(15:11-15:12),StringStart(15:13-15:14),StringPart(15:14-15:18),StringEnd(15:18-15:19),EndOfFile(15:19-15:19),
~~~
# PARSE
~~~clojure
(file @1.1-15.19
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.13
			(header @3.1-3.8 (name "BadBase")
				(args))
			(_))
		(s-type-decl @5.1-5.22
			(header @5.1-5.11 (name "BadDerived")
				(args))
			(ty @5.15-5.22 (name "BadBase")))
		(s-type-anno @7.1-7.19 (name "value")
			(ty @7.9-7.19 (name "BadDerived")))
		(s-decl @8.1-8.15
			(p-ident @8.1-8.6 (raw "value"))
			(e-string @8.9-8.15
				(e-string-part @8.10-8.14 (raw "test"))))
		(s-type-decl @10.1-10.16
			(header @10.1-10.9 (name "GoodBase")
				(args))
			(ty @10.13-10.16 (name "Str")))
		(s-type-decl @12.1-12.24
			(header @12.1-12.12 (name "GoodDerived")
				(args))
			(ty @12.16-12.24 (name "GoodBase")))
		(s-type-anno @14.1-14.24 (name "goodValue")
			(ty @14.13-14.24 (name "GoodDerived")))
		(s-decl @15.1-15.19
			(p-ident @15.1-15.10 (raw "goodValue"))
			(e-string @15.13-15.19
				(e-string-part @15.14-15.18 (raw "test"))))))
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
		(e-string @8.9-8.15
			(e-literal @8.10-8.14 (string "test")))
		(annotation @8.1-8.6
			(declared-type
				(ty @7.9-7.19 (name "BadDerived")))))
	(d-let
		(p-assign @15.1-15.10 (ident "goodValue"))
		(e-string @15.13-15.19
			(e-literal @15.14-15.18 (string "test")))
		(annotation @15.1-15.10
			(declared-type
				(ty @14.13-14.24 (name "GoodDerived")))))
	(s-nominal-decl @3.1-3.13
		(ty-header @3.1-3.8 (name "BadBase"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @5.1-5.22
		(ty-header @5.1-5.11 (name "BadDerived"))
		(ty @5.15-5.22 (name "BadBase")))
	(s-nominal-decl @10.1-10.16
		(ty-header @10.1-10.9 (name "GoodBase"))
		(ty @10.13-10.16 (name "Str")))
	(s-nominal-decl @12.1-12.24
		(ty-header @12.1-12.12 (name "GoodDerived"))
		(ty @12.16-12.24 (name "GoodBase"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.6 (type "Error"))
		(patt @15.1-15.10 (type "Error")))
	(type_decls
		(nominal @3.1-3.13 (type "Error")
			(ty-header @3.1-3.8 (name "BadBase")))
		(nominal @5.1-5.22 (type "Error")
			(ty-header @5.1-5.11 (name "BadDerived")))
		(nominal @10.1-10.16 (type "GoodBase")
			(ty-header @10.1-10.9 (name "GoodBase")))
		(nominal @12.1-12.24 (type "Error")
			(ty-header @12.1-12.12 (name "GoodDerived"))))
	(expressions
		(expr @8.9-8.15 (type "Error"))
		(expr @15.13-15.19 (type "Error"))))
~~~
