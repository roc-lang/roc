# META
~~~ini
description=Using a where alias as a type is rejected
type=snippet
~~~
# SOURCE
~~~roc
a.Stringable : where [a.to_str : a -> Str]

describe : Stringable -> Str
describe = |value| value.to_str()
~~~
# EXPECTED
WHERE ALIAS USED AS A TYPE - where_alias_in_type_position.md:3:12:3:22
# PROBLEMS

┌────────────────────────────┐
│ WHERE ALIAS USED AS A TYPE ├─ Stringable is a where alias, not a type. ─────┐
└┬───────────────────────────┘                                                │
 │                                                                            │
 │  describe : Stringable -> Str                                              │
 │             ‾‾‾‾‾‾‾‾‾‾                                                     │
 └────────────────────────────────────── where_alias_in_type_position.md:3:12 ┘

    A where alias names a set of method constraints, so it constrains a type
    variable in a `where` clause rather than standing in for a type of its own.

# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
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
		(s-type-anno (name "describe")
			(ty-fn
				(ty (name "Stringable"))
				(ty (name "Str"))))
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
a.Stringable :  where [a.to_str : a -> Str]

describe : Stringable -> Str
describe = |value| value.to_str()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "describe"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Stringable") (local))
				(ty-lookup (name "Str") (builtin)))))
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
		(patt (type "Error -> Str")))
	(type_decls
		(where-alias (type "a where [a.to_str : a -> Str]")
			(ty-header (name "Stringable"))))
	(expressions
		(expr (type "Error -> Str"))))
~~~
