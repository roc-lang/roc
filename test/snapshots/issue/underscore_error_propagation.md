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
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**underscore_error_propagation.md:1:1:1:1:**
```roc
BadBase := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
UpperIdent,OpColonEqual,UpperIdent,
UpperIdent,OpColonEqual,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "BadBase")
				(args))
			(_))
		(s-type-decl
			(header (name "BadDerived")
				(args))
			(ty (name "BadBase")))
		(s-type-anno (name "value")
			(ty (name "BadDerived")))
		(s-decl
			(p-ident (raw "value"))
			(e-string
				(e-string-part (raw "test"))))
		(s-type-decl
			(header (name "GoodBase")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "GoodDerived")
				(args))
			(ty (name "GoodBase")))
		(s-type-anno (name "goodValue")
			(ty (name "GoodDerived")))
		(s-decl
			(p-ident (raw "goodValue"))
			(e-string
				(e-string-part (raw "test"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-string
			(e-literal (string "test")))
		(annotation
			(ty-lookup (name "BadDerived") (local))))
	(d-let
		(p-assign (ident "goodValue"))
		(e-string
			(e-literal (string "test")))
		(annotation
			(ty-lookup (name "GoodDerived") (local))))
	(s-nominal-decl
		(ty-header (name "BadBase"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "BadDerived"))
		(ty-lookup (name "BadBase") (local)))
	(s-nominal-decl
		(ty-header (name "GoodBase"))
		(ty-lookup (name "Str") (external-module "Str")))
	(s-nominal-decl
		(ty-header (name "GoodDerived"))
		(ty-lookup (name "GoodBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(nominal (type "BadBase")
			(ty-header (name "BadBase")))
		(nominal (type "BadDerived")
			(ty-header (name "BadDerived")))
		(nominal (type "GoodBase")
			(ty-header (name "GoodBase")))
		(nominal (type "GoodDerived")
			(ty-header (name "GoodDerived"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
