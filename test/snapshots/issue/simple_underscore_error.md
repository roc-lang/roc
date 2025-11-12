# META
~~~ini
description=Simple test for single underscore type becoming error type
type=snippet
~~~
# SOURCE
~~~roc
BadType := _

foo : BadType
foo = 42
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - simple_underscore_error.md:1:1:1:1
MISSING METHOD - simple_underscore_error.md:4:7:4:9
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**simple_underscore_error.md:1:1:1:1:**
```roc
BadType := _
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**MISSING METHOD**
This **from_int_digits** method is being called on the type **BadType**, which has no method with that name:
**simple_underscore_error.md:4:7:4:9:**
```roc
foo = 42
```
      ^^


**Hint: **For this to work, the type would need to have a method named **from_int_digits** associated with it in the type's declaration.

# TOKENS
~~~zig
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
			(header (name "BadType")
				(args))
			(_))
		(s-type-anno (name "foo")
			(ty (name "BadType")))
		(s-decl
			(p-ident (raw "foo"))
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
		(p-assign (ident "foo"))
		(e-num (value "42"))
		(annotation
			(ty-lookup (name "BadType") (local))))
	(s-nominal-decl
		(ty-header (name "BadType"))
		(ty-underscore)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "BadType")))
	(type_decls
		(nominal (type "BadType")
			(ty-header (name "BadType"))))
	(expressions
		(expr (type "BadType"))))
~~~
