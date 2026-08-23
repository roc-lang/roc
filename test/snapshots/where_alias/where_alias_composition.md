# META
~~~ini
description=A where alias can name other where aliases
type=snippet
~~~
# SOURCE
~~~roc
a.Showable : where [a.to_str : a -> Str]

a.Comparable : where [a.compare : a -> [LT, EQ, GT]]

a.Sortable : where [a.Showable, a.Comparable]

describe : a -> Str where [a.Sortable]
describe = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,CloseSquare,
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,Comma,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name ".Showable")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-type-decl
			(header (name ".Comparable")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "compare")
					(args
						(ty-var (raw "a")))
					(ty-tag-union
						(tags
							(ty (name "LT"))
							(ty (name "EQ"))
							(ty (name "GT")))))))
		(s-type-decl
			(header (name ".Sortable")
				(args))
			(ty-var (raw "a"))
			(where
				(alias (mod-of "a")
					(ty (name "Showable")))
				(alias (mod-of "a")
					(ty (name "Comparable")))))
		(s-type-anno (name "describe")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Sortable")))))
		(s-decl
			(p-ident (raw "describe"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "value")))
					(args))))))
~~~
# FORMATTED
~~~roc
a.Showable :  where [a.to_str : a -> Str]

a.Comparable :  where [a.compare : a -> [LT, EQ, GT]]

a.Sortable :  where [a.Showable, a.Comparable]

describe : a -> Str where [a.Sortable]
describe = |value| value.to_str()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-dispatch-call (method "to_str") (constraint-fn-var 278)
				(receiver
					(e-lookup-local
						(p-assign (ident "value"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Sortable") (local))))))
	(s-where-alias-decl
		(ty-header (name "Showable"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Str") (builtin)))))
	(s-where-alias-decl
		(ty-header (name "Comparable"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "compare")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-tag-union
					(ty-tag-name (name "LT"))
					(ty-tag-name (name "EQ"))
					(ty-tag-name (name "GT"))))))
	(s-where-alias-decl
		(ty-header (name "Sortable"))
		(ty-rigid-var (name "a"))
		(where
			(alias
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Showable") (local)))
			(alias
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Comparable") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str where [a.compare : a -> [EQ, GT, LT], a.to_str : a -> Str]")))
	(type_decls
		(where-alias (type "a where [a.to_str : a -> Str]")
			(ty-header (name "Showable")))
		(where-alias (type "a where [a.compare : a -> [EQ, GT, LT]]")
			(ty-header (name "Comparable")))
		(where-alias (type "a where [a.compare : a -> [EQ, GT, LT], a.to_str : a -> Str]")
			(ty-header (name "Sortable"))))
	(expressions
		(expr (type "a -> Str where [a.compare : a -> [EQ, GT, LT], a.to_str : a -> Str]"))))
~~~
