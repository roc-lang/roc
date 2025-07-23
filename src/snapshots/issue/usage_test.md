# META
~~~ini
description=Test if usage affects error type conversion
type=file
~~~
# SOURCE
~~~roc
module []

UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**

**Underscore in Type Alias**
Underscore cannot be used in a type alias declaration:
**usage_test.md:1:1:1:1:**
```roc
module []
```



**UNDERSCORE IN TYPE ALIAS**

**Underscore in Type Alias**
Underscore cannot be used in a type alias declaration:
**usage_test.md:1:1:1:1:**
```roc
module []
```



# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
UpperIdent(3:1-3:11),OpColonEqual(3:12-3:14),Underscore(3:15-3:16),
UpperIdent(5:1-5:9),OpColonEqual(5:10-5:12),Underscore(5:13-5:14),
LowerIdent(7:1-7:6),OpColon(7:7-7:8),UpperIdent(7:9-7:17),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),Int(8:9-8:11),EndOfFile(8:11-8:11),
~~~
# PARSE
~~~clojure
(file @1.1-8.11
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-decl @3.1-3.16
			(header @3.1-3.11 (name "UnusedType")
				(args))
			(_))
		(s-type-decl @5.1-5.14
			(header @5.1-5.9 (name "UsedType")
				(args))
			(_))
		(s-type-anno @7.1-7.17 (name "value")
			(ty @7.9-7.17 (name "UsedType")))
		(s-decl @8.1-8.11
			(p-ident @8.1-8.6 (raw "value"))
			(e-int @8.9-8.11 (raw "42")))))
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
			(e-int @8.9-8.11 (value "42")))
		(annotation
			(annotation
				(type-anno
					(ty @7.9-7.17 (name "UsedType"))))))
	(s-nominal-decl @3.1-3.16
		(type-header (name "UnusedType"))
		(ty-underscore @1.1-1.1))
	(s-nominal-decl @5.1-5.14
		(type-header (name "UsedType"))
		(ty-underscore @1.1-1.1)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @8.1-8.6 (type "Error")))
	(type_decls
		(nominal @3.1-3.16 (type "Error")
			(type-header (name "UnusedType")))
		(nominal @5.1-5.14 (type "Error")
			(type-header (name "UsedType"))))
	(expressions
		(expr @8.9-8.11 (type "Error"))))
~~~
