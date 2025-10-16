# META
~~~ini
description=Example of a nominal tag union with a payload
type=snippet
~~~
# SOURCE
~~~roc
Maybe(a) := [Some(a), None]

some1 : a -> Maybe(a)
some1 = |a| Maybe.Some(a)

none1 : Maybe(_a)
none1 = Maybe.None

some2 = |a| Maybe.Some(a)

none2 = Maybe.None
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(ty-var (raw "a")))
					(ty (name "None")))))
		(s-type-anno (name "some1")
			(ty-fn
				(ty-var (raw "a"))
				(ty-apply
					(ty (name "Maybe"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "some1"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-apply
					(e-tag (raw "Maybe.Some"))
					(e-ident (raw "a")))))
		(s-type-anno (name "none1")
			(ty-apply
				(ty (name "Maybe"))
				(underscore-ty-var (raw "_a"))))
		(s-decl
			(p-ident (raw "none1"))
			(e-tag (raw "Maybe.None")))
		(s-decl
			(p-ident (raw "some2"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-apply
					(e-tag (raw "Maybe.Some"))
					(e-ident (raw "a")))))
		(s-decl
			(p-ident (raw "none2"))
			(e-tag (raw "Maybe.None")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "some1"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-nominal (nominal "Maybe")
				(e-tag (name "Some")
					(args
						(e-lookup-local
							(p-assign (ident "a")))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-rigid-var (name "a"))
					(ty-apply (name "Maybe") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))))
	(d-let
		(p-assign (ident "none1"))
		(e-nominal (nominal "Maybe")
			(e-tag (name "None")))
		(annotation
			(declared-type
				(ty-apply (name "Maybe") (local)
					(ty-rigid-var (name "_a"))))))
	(d-let
		(p-assign (ident "some2"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-nominal (nominal "Maybe")
				(e-tag (name "Some")
					(args
						(e-lookup-local
							(p-assign (ident "a"))))))))
	(d-let
		(p-assign (ident "none2"))
		(e-nominal (nominal "Maybe")
			(e-tag (name "None"))))
	(s-nominal-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "None")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Maybe(a)"))
		(patt (type "Maybe(_a)"))
		(patt (type "a -> Maybe(a)"))
		(patt (type "Maybe(a)")))
	(type_decls
		(nominal (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "a -> Maybe(a)"))
		(expr (type "Maybe(_a)"))
		(expr (type "a -> Maybe(a)"))
		(expr (type "Maybe(a)"))))
~~~
