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
NIL
# PROBLEMS
NIL
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
		(e-nominal (nominal "Foo.Bar.Baz")
			(e-tag (name "X")))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Bar.Baz") (local)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar.Baz"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Bar.Baz")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(nominal (type "Foo.Bar.Baz")
			(ty-header (name "Foo.Bar.Baz"))))
	(expressions
		(expr (type "Foo.Bar.Baz"))))
~~~
