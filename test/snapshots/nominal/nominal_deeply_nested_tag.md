# META
~~~ini
description=Deeply nested tag constructor (three levels deep)
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        Baz := [X, Y, Z]
    }
}

x : Foo.Bar.Baz
x = Foo.Bar.Baz.X
~~~
# EXPECTED
MISSING NESTED TYPE - nominal_deeply_nested_tag.md:7:5:7:16
UNDECLARED TYPE - nominal_deeply_nested_tag.md:8:12:8:16
# PROBLEMS
**MISSING NESTED TYPE**
`Foo.Bar` is in scope, but it doesn't have a nested type named `Baz`.

It's referenced here:
**nominal_deeply_nested_tag.md:7:5:7:16:**
```roc
x : Foo.Bar.Baz
```
    ^^^^^^^^^^^


**UNDECLARED TYPE**
The type _Foo.Bar.Baz_ is not declared in this scope.

This type is referenced here:
**nominal_deeply_nested_tag.md:8:12:8:16:**
```roc
x = Foo.Bar.Baz.X
```
           ^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
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
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Something"))))
					(associated
						(s-type-decl
							(header (name "Baz")
								(args))
							(ty-tag-union
								(tags
									(ty (name "X"))
									(ty (name "Y"))
									(ty (name "Z")))))))))
		(s-type-anno (name "x")
			(ty (name "Foo.Bar.Baz")))
		(s-decl
			(p-ident (raw "x"))
			(e-tag (raw "Foo.Bar.Baz.X")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		Baz := [X, Y, Z]
	}
}

x : Foo.Bar.Baz
x = Foo.Bar.Baz.X
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-runtime-error (tag "undeclared_type"))
		(annotation
			(ty-malformed)))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "nominal_deeply_nested_tag.Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something"))))
	(s-nominal-decl
		(ty-header (name "nominal_deeply_nested_tag.Foo.Bar.Baz"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "nominal_deeply_nested_tag.Foo.Bar")))
		(nominal (type "Foo.Bar.Baz")
			(ty-header (name "nominal_deeply_nested_tag.Foo.Bar.Baz"))))
	(expressions
		(expr (type "Error"))))
~~~
