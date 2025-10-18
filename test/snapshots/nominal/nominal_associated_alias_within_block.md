# META
~~~ini
description=Type alias within associated block referencing another nested type
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]

    # Alias within the associated block
    Baz : Foo.Bar

    defaultBaz : Foo.Baz
    defaultBaz = Foo.Bar.X
}

external : Foo.Baz
external = Foo.defaultBaz
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
UpperIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
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
							(ty (name "X"))
							(ty (name "Y"))
							(ty (name "Z")))))
				(s-type-decl
					(header (name "Baz")
						(args))
					(ty (name "Foo.Bar")))
				(s-type-anno (name "defaultBaz")
					(ty (name "Foo.Baz")))
				(s-decl
					(p-ident (raw "defaultBaz"))
					(e-tag (raw "Foo.Bar.X")))))
		(s-type-anno (name "external")
			(ty (name "Foo.Baz")))
		(s-decl
			(p-ident (raw "external"))
			(e-ident (raw "Foo.defaultBaz")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
	Baz : Foo.Bar
	defaultBaz : Foo.Baz
	defaultBaz = Foo.Bar.X
}

external : Foo.Baz
external = Foo.defaultBaz
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "external"))
		(e-lookup-local
			(p-assign (ident "Foo.defaultBaz")))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Baz") (local)))))
	(d-let
		(p-assign (ident "Foo.defaultBaz"))
		(e-nominal (nominal "Foo.Bar")
			(e-tag (name "X")))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Baz") (local)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z"))))
	(s-alias-decl
		(ty-header (name "Foo.Baz"))
		(ty-lookup (name "Foo.Bar") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Baz"))
		(patt (type "Foo.Baz")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(alias (type "Foo.Baz")
			(ty-header (name "Foo.Baz"))))
	(expressions
		(expr (type "Foo.Baz"))
		(expr (type "Foo.Baz"))))
~~~
