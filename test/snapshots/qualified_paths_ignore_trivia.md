# META
~~~ini
description=Trivia between uppercase-led qualified path segments does not change lookup, tag, or nominal construction semantics
type=snippet
~~~
# SOURCE
~~~roc
Outer := [OuterTag].{
	Middle := [MiddleTag].{
		Inner := U8.{
			value : U8
			value = 5
		}
	}
}

nestedValue : U8
nestedValue =
	Outer
		.Middle
		.Inner
		.value

nestedTag : Outer.Middle
nestedTag =
	Outer
		.Middle
		.MiddleTag

nestedNominal : Outer.Middle.Inner
nestedNominal =
	Outer
		.Middle
		.Inner
		.(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,UpperIdent,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,
UpperIdent,
DotUpperIdent,
DotUpperIdent,
DotLowerIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,
UpperIdent,
DotUpperIdent,
DotUpperIdent,
LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,
UpperIdent,
DotUpperIdent,
DotUpperIdent,
Dot,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Outer")
				(args))
			(ty-tag-union
				(tags
					(ty (name "OuterTag"))))
			(associated
				(s-type-decl
					(header (name "Middle")
						(args))
					(ty-tag-union
						(tags
							(ty (name "MiddleTag"))))
					(associated
						(s-type-decl
							(header (name "Inner")
								(args))
							(ty (name "U8"))
							(associated
								(s-type-anno (name "value")
									(ty (name "U8")))
								(s-decl
									(p-ident (raw "value"))
									(e-int (raw "5")))))))))
		(s-type-anno (name "nestedValue")
			(ty (name "U8")))
		(s-decl
			(p-ident (raw "nestedValue"))
			(e-ident (raw "Outer.Middle.Inner.value")))
		(s-type-anno (name "nestedTag")
			(ty (name "Outer.Middle")))
		(s-decl
			(p-ident (raw "nestedTag"))
			(e-tag (raw "Outer.Middle.MiddleTag")))
		(s-type-anno (name "nestedNominal")
			(ty (name "Outer.Middle.Inner")))
		(s-decl
			(p-ident (raw "nestedNominal"))
			(e-nominal-apply
				(mapper (e-tag (raw "Outer.Middle.Inner")))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
Outer := [OuterTag].{
	Middle := [MiddleTag].{
		Inner := U8.{
			value : U8
			value = 5
		}
	}
}

nestedValue : U8
nestedValue =
	Outer.Middle.Inner.value

nestedTag : Outer.Middle
nestedTag =
	Outer.Middle.MiddleTag

nestedNominal : Outer.Middle.Inner
nestedNominal =
	Outer.Middle.Inner.(5)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "qualified_paths_ignore_trivia.Outer.Middle.Inner.value"))
		(e-num (value "5"))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "nestedValue"))
		(e-lookup-local
			(p-assign (ident "qualified_paths_ignore_trivia.Outer.Middle.Inner.value")))
		(annotation
			(ty-lookup (name "U8") (builtin))))
	(d-let
		(p-assign (ident "nestedTag"))
		(e-nominal (nominal "qualified_paths_ignore_trivia.Outer.Middle")
			(e-tag (name "MiddleTag")))
		(annotation
			(ty-lookup (name "Outer.Middle") (local))))
	(d-let
		(p-assign (ident "nestedNominal"))
		(e-nominal (nominal "qualified_paths_ignore_trivia.Outer.Middle.Inner")
			(e-num (value "5")))
		(annotation
			(ty-lookup (name "Outer.Middle.Inner") (local))))
	(s-nominal-decl
		(ty-header (name "Outer"))
		(ty-tag-union
			(ty-tag-name (name "OuterTag"))))
	(s-nominal-decl
		(ty-header (name "qualified_paths_ignore_trivia.Outer.Middle"))
		(ty-tag-union
			(ty-tag-name (name "MiddleTag"))))
	(s-nominal-decl
		(ty-header (name "qualified_paths_ignore_trivia.Outer.Middle.Inner"))
		(ty-lookup (name "U8") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8"))
		(patt (type "U8"))
		(patt (type "Outer.Middle"))
		(patt (type "Outer.Middle.Inner")))
	(type_decls
		(nominal (type "Outer")
			(ty-header (name "Outer")))
		(nominal (type "Outer.Middle")
			(ty-header (name "qualified_paths_ignore_trivia.Outer.Middle")))
		(nominal (type "Outer.Middle.Inner")
			(ty-header (name "qualified_paths_ignore_trivia.Outer.Middle.Inner"))))
	(expressions
		(expr (type "U8"))
		(expr (type "U8"))
		(expr (type "Outer.Middle"))
		(expr (type "Outer.Middle.Inner"))))
~~~
