# META
~~~ini
description=Self-references within associated blocks using unqualified names
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [X, Y, Z]

    defaultBar : Bar
    defaultBar = X

    transform : Bar -> Bar
    transform = |x| x

    useDefault = transform(defaultBar)
}

external : Foo.Bar
external = Foo.defaultBar
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
				(s-type-anno (name "defaultBar")
					(ty (name "Bar")))
				(s-decl
					(p-ident (raw "defaultBar"))
					(e-tag (raw "X")))
				(s-type-anno (name "transform")
					(ty-fn
						(ty (name "Bar"))
						(ty (name "Bar"))))
				(s-decl
					(p-ident (raw "transform"))
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))
				(s-decl
					(p-ident (raw "useDefault"))
					(e-apply
						(e-ident (raw "transform"))
						(e-ident (raw "defaultBar"))))))
		(s-type-anno (name "external")
			(ty (name "Foo.Bar")))
		(s-decl
			(p-ident (raw "external"))
			(e-ident (raw "Foo.defaultBar")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [X, Y, Z]
	defaultBar : Bar
	defaultBar = X
	transform : Bar -> Bar
	transform = |x| x
	useDefault = transform(defaultBar)
}

external : Foo.Bar
external = Foo.defaultBar
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "external"))
		(e-lookup-local
			(p-assign (ident "Foo.defaultBar")))
		(annotation
			(ty-lookup (name "Foo.Bar") (local))))
	(d-let
		(p-assign (ident "Foo.defaultBar"))
		(e-tag (name "X"))
		(annotation
			(ty-lookup (name "Bar") (local))))
	(d-let
		(p-assign (ident "Foo.transform"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Bar") (local))
				(ty-lookup (name "Bar") (local)))))
	(d-let
		(p-assign (ident "Foo.useDefault"))
		(e-call
			(e-lookup-local
				(p-assign (ident "Foo.transform")))
			(e-lookup-local
				(p-assign (ident "Foo.defaultBar")))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Bar"))
		(patt (type "Foo.Bar"))
		(patt (type "Foo.Bar -> Foo.Bar"))
		(patt (type "Foo.Bar")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Foo.Bar"))
		(expr (type "Foo.Bar"))
		(expr (type "Foo.Bar -> Foo.Bar"))
		(expr (type "Foo.Bar"))))
~~~
