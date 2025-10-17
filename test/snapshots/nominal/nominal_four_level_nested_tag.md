# META
~~~ini
description=Four-level nested tag constructor (Foo.Bar.Baz.Qux.Tag)
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A].{
    Bar := [B].{
        Baz := [C].{
            Qux := [X, Y, Z]
        }
    }
}

value : Foo.Bar.Baz.Qux
value = Foo.Bar.Baz.Qux.Y
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
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
					(ty (name "A"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "B"))))
					(associated
						(s-type-decl
							(header (name "Baz")
								(args))
							(ty-tag-union
								(tags
									(ty (name "C"))))
							(associated
								(s-type-decl
									(header (name "Qux")
										(args))
									(ty-tag-union
										(tags
											(ty (name "X"))
											(ty (name "Y"))
											(ty (name "Z")))))))))))
		(s-type-anno (name "value")
			(ty (name "Foo.Bar.Baz.Qux")))
		(s-decl
			(p-ident (raw "value"))
			(e-tag (raw "Foo.Bar.Baz.Qux.Y")))))
~~~
# FORMATTED
~~~roc
Foo := [A].{
	Bar := [B].{
		Baz := [C].{
			Qux := [X, Y, Z]
		}
	}
}

value : Foo.Bar.Baz.Qux
value = Foo.Bar.Baz.Qux.Y
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-nominal (nominal "Foo.Bar.Baz.Qux")
			(e-tag (name "Y")))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Bar.Baz.Qux") (local)))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "B"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar.Baz"))
		(ty-tag-union
			(ty-tag-name (name "C"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar.Baz.Qux"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Bar.Baz.Qux")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(nominal (type "Foo.Bar.Baz")
			(ty-header (name "Foo.Bar.Baz")))
		(nominal (type "Foo.Bar.Baz.Qux")
			(ty-header (name "Foo.Bar.Baz.Qux"))))
	(expressions
		(expr (type "Foo.Bar.Baz.Qux"))))
~~~
