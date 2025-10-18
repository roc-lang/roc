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
UpperIdent,OpColonEqual,Underscore,
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
			(header (name "GoodAlias")
				(args))
			(ty (name "BadBase")))
		(s-type-anno (name "value")
			(ty (name "GoodAlias")))
		(s-decl
			(p-ident (raw "value"))
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
			(declared-type
				(ty-lookup (name "GoodAlias") (local)))))
	(s-nominal-decl
		(ty-header (name "BadBase"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "GoodAlias"))
		(ty-lookup (name "BadBase") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "BadBase")
			(ty-header (name "BadBase")))
		(nominal (type "GoodAlias")
			(ty-header (name "GoodAlias"))))
	(expressions
		(expr (type "Error"))))
~~~
