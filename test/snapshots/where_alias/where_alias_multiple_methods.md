# META
~~~ini
description=A where alias naming more than one method applies all of them
type=snippet
~~~
# SOURCE
~~~roc
a.Codec : where [
	a.to_str : a -> Str,
	a.from_str : Str -> a,
]

round_trip : a -> a where [a.Codec]
round_trip = |value| {
	Item : a
	Item.from_str(value.to_str())
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,Comma,
LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,OpArrow,LowerIdent,Comma,
CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name ".Codec")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))
				(method (mod-of "a") (name "from_str")
					(args
						(ty (name "Str")))
					(ty-var (raw "a")))))
		(s-type-anno (name "round_trip")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "a")))
			(where
				(alias (mod-of "a")
					(ty (name "Codec")))))
		(s-decl
			(p-ident (raw "round_trip"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-block
					(statements
						(s-type-decl
							(header (name "Item")
								(args))
							(ty-var (raw "a")))
						(e-apply
							(e-ident (raw "Item.from_str"))
							(e-method-call (method ".to_str")
								(receiver
									(e-ident (raw "value")))
								(args)))))))))
~~~
# FORMATTED
~~~roc
a.Codec :
	where [
		a.to_str : a -> Str,
		a.from_str : Str -> a,
	]

round_trip : a -> a where [a.Codec]
round_trip = |value| {
	Item : a
	Item.from_str(value.to_str())
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "round_trip"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-block
				(s-type-var-alias (alias "Item") (type-var "a")
					(ty-rigid-var (name "a")))
				(e-type-dispatch-call (method "from_str") (type-dispatch-stmt 21) (constraint-fn-var 255)
					(args
						(e-dispatch-call (method "to_str") (constraint-fn-var 253)
							(receiver
								(e-lookup-local
									(p-assign (ident "value"))))
							(args))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Codec") (local))))))
	(s-where-alias-decl
		(ty-header (name "Codec"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Str") (builtin)))
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "from_str")
				(args
					(ty-lookup (name "Str") (builtin)))
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.from_str : Str -> a, a.to_str : a -> Str]")))
	(type_decls
		(where-alias (type "a where [a.from_str : Str -> a, a.to_str : a -> Str]")
			(ty-header (name "Codec"))))
	(expressions
		(expr (type "a -> a where [a.from_str : Str -> a, a.to_str : a -> Str]"))))
~~~
