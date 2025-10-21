# META
~~~ini
description=Type alias referencing associated nested type
type=snippet
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]
}

# Type alias to the nested type
MyBar : Foo.Bar

useMyBar : MyBar
useMyBar = Foo.Bar.X
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
CloseCurly,
UpperIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
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
							(ty (name "Z")))))))
		(s-type-decl
			(header (name "MyBar")
				(args))
			(ty (name "Foo.Bar")))
		(s-type-anno (name "useMyBar")
			(ty (name "MyBar")))
		(s-decl
			(p-ident (raw "useMyBar"))
			(e-tag (raw "Foo.Bar.X")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
}

# Type alias to the nested type
MyBar : Foo.Bar

useMyBar : MyBar
useMyBar = Foo.Bar.X
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "useMyBar"))
		(e-nominal (nominal "Foo.Bar")
			(e-tag (name "X")))
		(annotation
			(ty-lookup (name "MyBar") (local))))
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
		(ty-header (name "MyBar"))
		(ty-lookup (name "Foo.Bar") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyBar")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(alias (type "MyBar")
			(ty-header (name "MyBar"))))
	(expressions
		(expr (type "MyBar"))))
~~~
