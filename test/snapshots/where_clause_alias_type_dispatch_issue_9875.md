# META
~~~ini
description=Type dispatch through a top-level type alias resolves the target type's associated items (issue 9875)
type=snippet
~~~
# SOURCE
~~~roc
Thing := [MkThing(U64)].{
    from_u64 : U64 -> Thing
    from_u64 = |n| Thing.MkThing(n)

    value : Thing -> U64
    value = |Thing.MkThing(n)| n
}

ThingAlias : Thing

main : U64
main = ThingAlias.from_u64(41).value() + 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpBar,LowerIdent,
CloseCurly,
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Thing")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "MkThing"))
						(ty (name "U64")))))
			(associated
				(s-type-anno (name "from_u64")
					(ty-fn
						(ty (name "U64"))
						(ty (name "Thing"))))
				(s-decl
					(p-ident (raw "from_u64"))
					(e-lambda
						(args
							(p-ident (raw "n")))
						(e-apply
							(e-tag (raw "Thing.MkThing"))
							(e-ident (raw "n")))))
				(s-type-anno (name "value")
					(ty-fn
						(ty (name "Thing"))
						(ty (name "U64"))))
				(s-decl
					(p-ident (raw "value"))
					(e-lambda
						(args
							(p-tag (raw ".MkThing")
								(p-ident (raw "n"))))
						(e-ident (raw "n"))))))
		(s-type-decl
			(header (name "ThingAlias")
				(args))
			(ty (name "Thing")))
		(s-type-anno (name "main")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "main"))
			(e-binop (op "+")
				(e-method-call (method ".value")
					(receiver
						(e-apply
							(e-ident (raw "ThingAlias.from_u64"))
							(e-int (raw "41"))))
					(args))
				(e-int (raw "1"))))))
~~~
# FORMATTED
~~~roc
Thing := [MkThing(U64)].{
	from_u64 : U64 -> Thing
	from_u64 = |n| Thing.MkThing(n)

	value : Thing -> U64
	value = |Thing.MkThing(n)| n
}

ThingAlias : Thing

main : U64
main = ThingAlias.from_u64(41).value() + 1
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "where_clause_alias_type_dispatch_issue_9875.Thing.from_u64"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-nominal (nominal "Thing")
				(e-tag (name "MkThing")
					(args
						(e-lookup-local
							(p-assign (ident "n")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U64") (builtin))
				(ty-lookup (name "Thing") (local)))))
	(d-let
		(p-assign (ident "where_clause_alias_type_dispatch_issue_9875.Thing.value"))
		(e-lambda
			(args
				(p-nominal
					(p-applied-tag)))
			(e-lookup-local
				(p-assign (ident "n"))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Thing") (local))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "main"))
		(e-dispatch-call (method "plus") (constraint-fn-var 297)
			(receiver
				(e-dispatch-call (method "value") (constraint-fn-var 262)
					(receiver
						(e-call (constraint-fn-var 191)
							(e-lookup-local
								(p-assign (ident "where_clause_alias_type_dispatch_issue_9875.Thing.from_u64")))
							(e-num (value "41"))))
					(args)))
			(args
				(e-num (value "1"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-nominal-decl
		(ty-header (name "Thing"))
		(ty-tag-union
			(ty-tag-name (name "MkThing")
				(ty-lookup (name "U64") (builtin)))))
	(s-alias-decl
		(ty-header (name "ThingAlias"))
		(ty-lookup (name "Thing") (local))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64 -> Thing"))
		(patt (type "Thing -> U64"))
		(patt (type "U64")))
	(type_decls
		(nominal (type "Thing")
			(ty-header (name "Thing")))
		(alias (type "ThingAlias")
			(ty-header (name "ThingAlias"))))
	(expressions
		(expr (type "U64 -> Thing"))
		(expr (type "Thing -> U64"))
		(expr (type "U64"))))
~~~
