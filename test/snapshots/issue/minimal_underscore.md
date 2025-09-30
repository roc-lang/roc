# META
~~~ini
description=Minimal test - underscore type should become error type
type=file:BadType.roc
~~~
# SOURCE
~~~roc
BadType := _
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - minimal_underscore.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**minimal_underscore.md:1:1:1:1:**
```roc
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent(1:1-1:8),OpColonEqual(1:9-1:11),Underscore(1:12-1:13),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.13
	(type-module @1.1-1.8)
	(statements
		(s-type-decl @1.1-1.13
			(header @1.1-1.8 (name "BadType")
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
	(s-nominal-decl @1.1-1.13
		(ty-header @1.1-1.8 (name "BadType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @1.1-1.13 (type "Error")
			(ty-header @1.1-1.8 (name "BadType"))))
	(expressions))
~~~
