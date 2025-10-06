# META
~~~ini
description=Minimal test - underscore type should become error type
type=file:MinimalUnderscore.roc
~~~
# SOURCE
~~~roc
MinimalUnderscore := {}

BadType := _
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - minimal_underscore.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**minimal_underscore.md:1:1:1:1:**
```roc
MinimalUnderscore := {}
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent(1:1-1:18),OpColonEqual(1:19-1:21),OpenCurly(1:22-1:23),CloseCurly(1:23-1:24),
UpperIdent(3:1-3:8),OpColonEqual(3:9-3:11),Underscore(3:12-3:13),
EndOfFile(4:1-4:1),
~~~
# PARSE
~~~clojure
(file @1.1-3.13
	(type-module @1.1-1.18)
	(statements
		(s-type-decl @1.1-1.24
			(header @1.1-1.18 (name "MinimalUnderscore")
				(args))
			(ty-record @1.22-1.24))
		(s-type-decl @3.1-3.13
			(header @3.1-3.8 (name "BadType")
				(args))
			(_))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl @1.1-1.24
		(ty-header @1.1-1.18 (name "MinimalUnderscore"))
		(ty-record @1.22-1.24))
	(s-nominal-decl @3.1-3.13
		(ty-header @3.1-3.8 (name "BadType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.24 (type "MinimalUnderscore")
			(ty-header @1.1-1.18 (name "MinimalUnderscore")))
		(nominal @3.1-3.13 (type "BadType")
			(ty-header @3.1-3.8 (name "BadType"))))
	(expressions))
~~~
