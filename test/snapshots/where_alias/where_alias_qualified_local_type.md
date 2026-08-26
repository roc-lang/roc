# META
~~~ini
description=A where alias nested in a local type is referenced through its qualified name
type=snippet
~~~
# SOURCE
~~~roc
Holder := {}.{
	a.Show : where [a.to_str : a -> Str]
}

describe : a -> Str where [a.Holder.Show]
describe = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
CloseCurly,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Holder")
				(args))
			(ty-record)
			(associated
				(s-type-decl
					(header (name ".Show")
						(args))
					(ty-var (raw "a"))
					(where
						(method (mod-of "a") (name "to_str")
							(args
								(ty-var (raw "a")))
							(ty (name "Str")))))))
		(s-type-anno (name "describe")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Holder.Show")))))
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
Holder := {}.{
	a.Show :  where [a.to_str : a -> Str]
}

describe : a -> Str where [a.Holder.Show]
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
			(e-dispatch-call (method "to_str") (constraint-fn-var 245)
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
					(ty-lookup (name "Holder.Show") (local))))))
	(s-nominal-decl
		(ty-header (name "Holder"))
		(ty-record))
	(s-where-alias-decl
		(ty-header (name "where_alias_qualified_local_type.Holder.Show"))
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
		(nominal (type "Holder")
			(ty-header (name "Holder")))
		(where-alias (type "a where [a.to_str : a -> Str]")
			(ty-header (name "where_alias_qualified_local_type.Holder.Show"))))
	(expressions
		(expr (type "a -> Str where [a.to_str : a -> Str]"))))
~~~
