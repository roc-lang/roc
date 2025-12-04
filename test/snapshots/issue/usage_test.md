# META
~~~ini
description=Test if usage affects error type conversion
type=snippet
~~~
# SOURCE
~~~roc
UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
TYPE REDECLARED - usage_test.md:3:1:3:14
UNDERSCORE IN TYPE ALIAS - usage_test.md:1:1:1:1
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**usage_test.md:1:1:1:1:**
```roc
UnusedType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**TYPE REDECLARED**
The type _UsedType_ is being redeclared.

The redeclaration is here:
**usage_test.md:3:1:3:14:**
```roc
UsedType := _
```
^^^^^^^^^^^^^

But _UsedType_ was already declared here:
**usage_test.md:3:1:3:14:**
```roc
UsedType := _
```
^^^^^^^^^^^^^


**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**usage_test.md:1:1:1:1:**
```roc
UnusedType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,Underscore,
UpperIdent,OpColonEqual,Underscore,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "UnusedType")
				(args))
			(_))
		(s-type-decl
			(header (name "UsedType")
				(args))
			(_))
		(s-type-anno (name "value")
			(ty (name "UsedType")))
		(s-decl
			(p-ident (raw "value"))
			(e-int (raw "42")))))
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
		(e-num (value "42"))
		(annotation
			(ty-lookup (name "UsedType") (local))))
	(s-nominal-decl
		(ty-header (name "UnusedType"))
		(ty-underscore))
	(s-nominal-decl
		(ty-header (name "UsedType"))
		(ty-underscore)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(type_decls
		(nominal (type "UnusedType")
			(ty-header (name "UnusedType")))
		(nominal (type "UsedType")
			(ty-header (name "UsedType"))))
	(expressions
		(expr (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
