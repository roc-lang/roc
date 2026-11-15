# META
~~~ini
description=An exposed field typed through a public alias whose private alias chain permutes its parameters back into the order of a private nominal type's parameters does not warn: the alias renames that nominal type
type=file:Api.roc
~~~
# SOURCE
~~~roc
Api := { item : Api.Item(U64, Str) }.{
    Item(x, y) : Mid(y, x)
}
Mid(p, q) : Pair(q, p)
Pair(a, b) := { first : a, second : b }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,CloseCurly,Dot,OpenCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Api")
				(args))
			(ty-record
				(anno-record-field (name "item")
					(ty-apply
						(ty (name "Api.Item"))
						(ty (name "U64"))
						(ty (name "Str")))))
			(associated
				(s-type-decl
					(header (name "Item")
						(args
							(ty-var (raw "x"))
							(ty-var (raw "y"))))
					(ty-apply
						(ty (name "Mid"))
						(ty-var (raw "y"))
						(ty-var (raw "x"))))))
		(s-type-decl
			(header (name "Mid")
				(args
					(ty-var (raw "p"))
					(ty-var (raw "q"))))
			(ty-apply
				(ty (name "Pair"))
				(ty-var (raw "q"))
				(ty-var (raw "p"))))
		(s-type-decl
			(header (name "Pair")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-record
				(anno-record-field (name "first")
					(ty-var (raw "a")))
				(anno-record-field (name "second")
					(ty-var (raw "b")))))))
~~~
# FORMATTED
~~~roc
Api := { item : Api.Item(U64, Str) }.{
	Item(x, y) : Mid(y, x)
}

Mid(p, q) : Pair(q, p)

Pair(a, b) := { first : a, second : b }
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "Api"))
		(ty-record
			(field (field "item")
				(ty-apply (name "Api.Item") (local)
					(ty-lookup (name "U64") (builtin))
					(ty-lookup (name "Str") (builtin))))))
	(s-alias-decl
		(ty-header (name "Api.Item")
			(ty-args
				(ty-rigid-var (name "x"))
				(ty-rigid-var (name "y"))))
		(ty-apply (name "Mid") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "y")))
			(ty-rigid-var-lookup (ty-rigid-var (name "x")))))
	(s-alias-decl
		(ty-header (name "Mid")
			(ty-args
				(ty-rigid-var (name "p"))
				(ty-rigid-var (name "q"))))
		(ty-apply (name "Pair") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "q")))
			(ty-rigid-var-lookup (ty-rigid-var (name "p")))))
	(s-nominal-decl
		(ty-header (name "Pair")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-record
			(field (field "first")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(field (field "second")
				(ty-rigid-var-lookup (ty-rigid-var (name "b")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "Api")
			(ty-header (name "Api")))
		(alias (type "Api.Item(x, y)")
			(ty-header (name "Api.Item")
				(ty-args
					(ty-rigid-var (name "x"))
					(ty-rigid-var (name "y")))))
		(alias (type "Mid(p, q)")
			(ty-header (name "Mid")
				(ty-args
					(ty-rigid-var (name "p"))
					(ty-rigid-var (name "q")))))
		(nominal (type "Pair(a, b)")
			(ty-header (name "Pair")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions))
~~~
