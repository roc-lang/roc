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
The type _Foo.Bar.Baz_ is not declared in this scope.

This type is referenced here:
**nominal_undefined_deeply_nested_tag.md:5:12:5:16:**
```roc
x = Foo.Bar.Baz.X
```
           ^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:4),OpColonEqual(1:5-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:10),Comma(1:10-1:11),UpperIdent(1:12-1:13),Comma(1:13-1:14),UpperIdent(1:15-1:16),CloseSquare(1:16-1:17),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),UpperIdent(5:5-5:8),NoSpaceDotUpperIdent(5:8-5:12),NoSpaceDotUpperIdent(5:12-5:16),NoSpaceDotUpperIdent(5:16-5:18),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.18
	(type-module @1.1-1.4)
	(statements
		(s-type-decl @1.1-1.17
			(header @1.1-1.4 (name "Foo")
				(args))
			(ty-tag-union @1.8-1.17
				(tags
					(ty @1.9-1.10 (name "A"))
					(ty @1.12-1.13 (name "B"))
					(ty @1.15-1.16 (name "C")))))
		(s-decl @5.1-5.18
			(p-ident @5.1-5.2 (raw "x"))
			(e-tag @5.5-5.18 (raw "Foo.Bar.Baz.X")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.2 (ident "x"))
		(e-runtime-error (tag "undeclared_type")))
	(s-nominal-decl @1.1-1.17
		(ty-header @1.1-1.4 (name "Foo"))
		(ty-tag-union @1.8-1.17
			(ty-tag-name @1.9-1.10 (name "A"))
			(ty-tag-name @1.12-1.13 (name "B"))
			(ty-tag-name @1.15-1.16 (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.2 (type "Error")))
	(type_decls
		(nominal @1.1-1.17 (type "Foo")
			(ty-header @1.1-1.4 (name "Foo"))))
	(expressions
		(expr @5.12-5.16 (type "Error"))))
~~~
