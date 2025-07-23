# META
~~~ini
description=Minimal test - underscore type should become error type
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - minimal_underscore.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**

**Underscore in Type Alias**
Underscore cannot be used in a type alias declaration:
**minimal_underscore.md:1:1:1:1:**
```roc
module []
```



# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:8),OpColonEqual(3:9-3:11),Underscore(3:12-3:13),EndOfFile(3:13-3:13),
~~~
# PARSE
~~~clojure
(file @1.1-3.13
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
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
	(s-nominal-decl @3.1-3.13
		(type-header (name "BadType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal @3.1-3.13 (type "Error")
			(type-header (name "BadType"))))
	(expressions))
~~~
