# META
~~~ini
description=Test error propagation - aliases that reference error types should not propagate errors
type=snippet
~~~
# SOURCE
~~~roc
BadBase := _

GoodAlias := BadBase

value : GoodAlias
value = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - test_error_propagation.md:1:1:1:1
TYPE MISMATCH - test_error_propagation.md:6:9:6:15
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**test_error_propagation.md:1:1:1:1:**
```roc
BadBase := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_error_propagation.md:6:9:6:15:**
```roc
value = "test"
```
        ^^^^^^

It has the type:
    _Str_

But the type annotation says it should have the type:
    _GoodAlias_

# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),Underscore(1:12-1:13),
UpperIdent(3:1-3:10),OpColonEqual(3:11-3:13),UpperIdent(3:14-3:21),
LowerIdent(5:1-5:6),OpColon(5:7-5:8),UpperIdent(5:9-5:18),
LowerIdent(6:1-6:6),OpAssign(6:7-6:8),StringStart(6:9-6:10),StringPart(6:10-6:14),StringEnd(6:14-6:15),
EndOfFile(7:1-7:1),
~~~
# PARSE
~~~clojure
(file @1.1-6.15
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.8 (name "BadBase")
				(args))
			(_))
		(s-type-decl @3.1-3.21
			(header @3.1-3.10 (name "GoodAlias")
				(args))
			(ty @3.14-3.21 (name "BadBase")))
		(s-type-anno @5.1-5.18 (name "value")
			(ty @5.9-5.18 (name "GoodAlias")))
		(s-decl @6.1-6.15
			(p-ident @6.1-6.6 (raw "value"))
			(e-string @6.9-6.15
				(e-string-part @6.10-6.14 (raw "test"))))))
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
				(ty-lookup @5.9-5.18 (name "GoodAlias") (local)))))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.8 (name "BadBase"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @3.1-3.21
		(ty-header @3.1-3.10 (name "GoodAlias"))
		(ty-lookup @3.14-3.21 (name "BadBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.6 (type "Error")))
	(type_decls
		(nominal @1.1-1.13 (type "BadBase")
			(ty-header @1.1-1.8 (name "BadBase")))
		(nominal @3.1-3.21 (type "GoodAlias")
			(ty-header @3.1-3.10 (name "GoodAlias"))))
	(expressions
		(expr @6.9-6.15 (type "Error"))))
~~~
