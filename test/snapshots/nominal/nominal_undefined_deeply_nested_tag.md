# META
~~~ini
description=Error for undefined deeply nested tag (no fallback to module)
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A, B, C]

# This should error as "undeclared type Foo.Bar.Baz"
# NOT try to interpret it as a module import
x = Foo.Bar.Baz.X
~~~
# EXPECTED
UNDECLARED TYPE - nominal_undefined_deeply_nested_tag.md:5:12:5:16
# PROBLEMS
**UNDECLARED TYPE**
Cannot resolve qualified type _Foo.Bar.Baz_.

This type is referenced here:
**nominal_undefined_deeply_nested_tag.md:5:12:5:16:**
```roc
x = Foo.Bar.Baz.X
```
           ^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B"))
					(ty (name "C")))))
		(s-decl
			(p-ident (raw "x"))
			(e-tag (raw "Foo.Bar.Baz.X")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "undeclared_type")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Error"))))
~~~
