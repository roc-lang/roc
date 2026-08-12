# META
~~~ini
description=A where clause naming an ordinary type instead of a where alias is rejected
type=snippet
~~~
# SOURCE
~~~roc
Wrapper(a) : List(a)

describe : a -> Str where [a.Wrapper]
describe = |value| value.to_str()
~~~
# EXPECTED
NOT A WHERE ALIAS - where_alias_not_a_where_alias.md:3:29:3:37
# PROBLEMS

┌───────────────────┐
│ NOT A WHERE ALIAS ├─ A where clause can only name a where alias, but ───────┐
└┬──────────────────┘  Wrapper is a type.                                     │
 │                                                                            │
 │  describe : a -> Str where [a.Wrapper]                                     │
 │                              ‾‾‾‾‾‾‾‾                                      │
 └───────────────────────────────────── where_alias_not_a_where_alias.md:3:29 ┘

    A where alias names a set of method constraints, declared like `a.Sortable
    : where [a.compare : a -> [LT, EQ, GT]]` and written in a where clause as
    `where [a.Sortable]`

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
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
			(header (name "Wrapper")
				(args
					(ty-var (raw "a"))))
			(ty-apply
				(ty (name "List"))
				(ty-var (raw "a"))))
		(s-type-anno (name "describe")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Wrapper")))))
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
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Wrapper") (local))))))
	(s-alias-decl
		(ty-header (name "Wrapper")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-apply (name "List") (builtin)
			(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str")))
	(type_decls
		(alias (type "Wrapper(a)")
			(ty-header (name "Wrapper")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions
		(expr (type "Error -> Str"))))
~~~
