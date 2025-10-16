# META
~~~ini
description=Mixed usage of types and declarations from associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [A, B, C]

    defaultBar = Bar.A

    transform : Foo.Bar -> Foo.Bar
    transform = |x| x
}

result : Foo.Bar
result = Foo.transform(Foo.defaultBar)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,OpArrow,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,UpperIdent,NoSpaceDotLowerIdent,CloseRound,
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
							(ty (name "A"))
							(ty (name "B"))
							(ty (name "C")))))
				(s-decl
					(p-ident (raw "defaultBar"))
					(e-tag (raw "Bar.A")))
				(s-type-anno (name "transform")
					(ty-fn
						(ty (name "Foo.Bar"))
						(ty (name "Foo.Bar"))))
				(s-decl
					(p-ident (raw "transform"))
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-ident (raw "x"))))))
		(s-type-anno (name "result")
			(ty (name "Foo.Bar")))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "Foo.transform"))
				(e-ident (raw "Foo.defaultBar"))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [A, B, C]
	defaultBar = Bar.A
	transform : Foo.Bar -> Foo.Bar
	transform = |x| x
}

result : Foo.Bar
result = Foo.transform(Foo.defaultBar)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "Foo.transform")))
			(e-lookup-local
				(p-assign (ident "Foo.defaultBar"))))
		(annotation
			(declared-type
				(ty-lookup (name "Foo.Bar") (local)))))
	(d-let
		(p-assign (ident "Foo.defaultBar"))
		(e-nominal (nominal "Foo.Bar")
			(e-tag (name "A"))))
	(d-let
		(p-assign (ident "Foo.transform"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-lookup-local
				(p-assign (ident "x"))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "Foo.Bar") (local))
					(ty-lookup (name "Foo.Bar") (local))))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Foo.Bar"))
		(patt (type "Foo.Bar"))
		(patt (type "Foo.Bar -> Foo.Bar")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Foo.Bar"))
		(expr (type "Foo.Bar"))
		(expr (type "Foo.Bar -> Foo.Bar"))))
~~~
