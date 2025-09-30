# META
~~~ini
description=Simple test for single underscore type becoming error type
type=file:BadType.roc
~~~
# SOURCE
~~~roc
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
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),Underscore(1:12-1:13),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:14),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.9
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.8 (name "BadType")
				(args))
			(_))
		(s-type-anno @3.1-3.14 (name "foo")
			(ty @3.7-3.14 (name "BadType")))
		(s-decl @4.1-4.9
			(p-ident @4.1-4.4 (raw "foo"))
			(e-int @4.7-4.9 (raw "42")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "foo"))
		(e-int @4.7-4.9 (value "42"))
		(annotation @4.1-4.4
			(declared-type
				(ty @3.7-3.14 (name "BadType")))))
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.8 (name "BadType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Error")))
	(type_decls
		(nominal @1.1-1.13 (type "Error")
			(ty-header @1.1-1.8 (name "BadType"))))
	(expressions
		(expr @4.7-4.9 (type "Error"))))
~~~
