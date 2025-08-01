# META
~~~ini
description=Simple test for single underscore type becoming error type
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _

foo : BadType
foo = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - simple_underscore_error.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**simple_underscore_error.md:1:1:1:1:**
```roc
module []
```


Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:8),OpColonEqual(3:9-3:11),Underscore(3:12-3:13),
LowerIdent(5:1-5:4),OpColon(5:5-5:6),UpperIdent(5:7-5:14),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),Int(6:7-6:9),EndOfFile(6:9-6:9),
~~~
# PARSE
~~~clojure
(file @1.1-6.9
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.13
			(header @3.1-3.8 (name "BadType")
				(args))
			(_))
		(s-type-anno @5.1-5.14 (name "foo")
			(ty @5.7-5.14 (name "BadType")))
		(s-decl @6.1-6.9
			(p-ident @6.1-6.4 (raw "foo"))
			(e-int @6.7-6.9 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.4 (ident "foo"))
		(e-int @6.7-6.9 (value "42"))
		(annotation @6.1-6.4
			(declared-type
				(ty @5.7-5.14 (name "BadType")))))
	(s-nominal-decl @3.1-3.13
		(ty-header @3.1-3.8 (name "BadType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.4 (type "Error")))
	(type_decls
		(nominal @3.1-3.13 (type "Error")
			(ty-header @3.1-3.8 (name "BadType"))))
	(expressions
		(expr @6.7-6.9 (type "Error"))))
~~~
