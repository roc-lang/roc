# META
~~~ini
description=A where alias names a set of method constraints that a signature can apply
type=snippet
~~~
# SOURCE
~~~roc
a.Stringable : where [a.to_str : a -> Str]

stringify : a -> Str where [a.Stringable]
stringify = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
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
			(header (name ".Stringable")
				(args))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-type-anno (name "stringify")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Stringable")))))
		(s-decl
			(p-ident (raw "stringify"))
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
a.Stringable :  where [a.to_str : a -> Str]

stringify : a -> Str where [a.Stringable]
stringify = |value| value.to_str()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "stringify"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-dispatch-call (method "to_str") (constraint-fn-var 240)
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
					(ty-lookup (name "Stringable") (local))))))
	(s-where-alias-decl
		(ty-header (name "Stringable"))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str where [a.to_str : a -> Str]")))
	(type_decls
		(where-alias (type "a where [a.to_str : a -> Str]")
			(ty-header (name "Stringable"))))
	(expressions
		(expr (type "a -> Str where [a.to_str : a -> Str]"))))
~~~
