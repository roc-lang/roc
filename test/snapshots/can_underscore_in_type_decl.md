# META
~~~ini
description=Test underscore used in type declaration (should produce error)
type=snippet
~~~
# SOURCE
~~~roc
MyType : _ -> Int
myValue : MyType
myValue = |x| x

main = myValue
~~~
# EXPECTED
UNDERSCORE IN TYPE ALIAS - can_underscore_in_type_decl.md:1:1:1:1
UNDECLARED TYPE - can_underscore_in_type_decl.md:1:15:1:18
# PROBLEMS
**UNDERSCORE IN TYPE ALIAS**
Underscores are not allowed in type alias declarations.

**can_underscore_in_type_decl.md:1:1:1:1:**
```roc
MyType : _ -> Int
```
^

Underscores in type annotations mean "I don't care about this type", which doesn't make sense when declaring a type. If you need a placeholder type variable, use a named type variable like `a` instead.

**UNDECLARED TYPE**
The type _Int_ is not declared in this scope.

This type is referenced here:
**can_underscore_in_type_decl.md:1:15:1:18:**
```roc
MyType : _ -> Int
```
              ^^^


# TOKENS
~~~zig
UpperIdent,OpColon,Underscore,OpArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty-fn
				(_)
				(ty (name "Int"))))
		(s-type-anno (name "myValue")
			(ty (name "MyType")))
		(s-decl
			(p-ident (raw "myValue"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-ident (raw "x"))))
		(s-decl
			(p-ident (raw "main"))
			(e-ident (raw "myValue")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myValue"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-lookup (name "MyType") (local))))
	(d-let
		(p-assign (ident "main"))
		(e-lookup-local
			(p-assign (ident "myValue"))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-fn (effectful false)
			(ty-underscore)
			(ty-malformed))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyType"))
		(patt (type "MyType")))
	(type_decls
		(alias (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "MyType"))
		(expr (type "MyType"))))
~~~
