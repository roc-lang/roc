# META
~~~ini
description=Test error propagation - aliases that reference error types should not propagate errors
type=file
~~~
# SOURCE
~~~roc
module []

BadBase := _

GoodAlias := BadBase

value : GoodAlias
value = "test"
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - test_error_propagation.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**

**Underscore in Type Alias**
Underscore cannot be used in a type alias declaration:
**test_error_propagation.md:1:1:1:1:**
```roc
module []
```



# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:8),OpColonEqual(3:9-3:11),Underscore(3:12-3:13),
UpperIdent(5:1-5:10),OpColonEqual(5:11-5:13),UpperIdent(5:14-5:21),
LowerIdent(7:1-7:6),OpColon(7:7-7:8),UpperIdent(7:9-7:18),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),StringStart(8:9-8:10),StringPart(8:10-8:14),StringEnd(8:14-8:15),EndOfFile(8:15-8:15),
~~~
# PARSE
~~~clojure
(file @1.1-8.15
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.13
			(header @3.1-3.8 (name "BadBase")
				(args))
			(_))
		(s-type-decl @5.1-5.21
			(header @5.1-5.10 (name "GoodAlias")
				(args))
			(ty @5.14-5.21 (name "BadBase")))
		(s-type-anno @7.1-7.18 (name "value")
			(ty @7.9-7.18 (name "GoodAlias")))
		(s-decl @8.1-8.15
			(p-ident @8.1-8.6 (raw "value"))
			(e-string @8.9-8.15
				(e-string-part @8.10-8.14 (raw "test"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @8.1-8.6 (ident "value")))
		(expr
			(e-string @8.9-8.15
				(e-literal @8.10-8.14 (string "test"))))
		(annotation
			(annotation
				(type-anno
					(ty @7.9-7.18 (name "GoodAlias"))))))
	(s-nominal-decl @3.1-3.13
		(type-header (name "BadBase"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @5.1-5.21
		(type-header (name "GoodAlias"))
		(ty @5.14-5.21 (name "BadBase"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.6 (type "Error")))
	(type_decls
		(nominal @3.1-3.13 (type "Error")
			(type-header (name "BadBase")))
		(nominal @5.1-5.21 (type "Error")
			(type-header (name "GoodAlias"))))
	(expressions
		(expr @8.9-8.15 (type "Error"))))
~~~
