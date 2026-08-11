# META
~~~ini
description=A where alias that names itself is rejected
type=snippet
~~~
# SOURCE
~~~roc
a.Looping : where [a.Looping, a.to_str : a -> Str]

describe : a -> Str where [a.Looping]
describe = |value| value.to_str()
~~~
# EXPECTED
RECURSIVE WHERE ALIAS - where_alias_recursive.md:1:21:1:29
MISSING METHOD - where_alias_recursive.md:4:26:4:32
# PROBLEMS
── ✗ recursive where alias ─────────────────────── where_alias_recursive.md:1:21

The where alias Looping names itself.

a.Looping : where [a.Looping, a.to_str : a -> Str]
                    ^^^^^^^^

A where alias is expanded where it is used, so it cannot reach itself, directly
or through other where aliases.

── ✗ missing method ────────────────────────────── where_alias_recursive.md:4:26

This to_str method is being called on a value whose type doesn't have that
method.

describe = |value| value.to_str()
                         ^^^^^^

The value's type, which does not have a method named to_str, is:

    a

Hint: For this to work, the type would need to have a method named to_str
associated with it in the type's declaration.

# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
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
			(header (name ".Looping")
				(args))
			(ty-var (raw "a"))
			(where
				(alias (mod-of "a")
					(ty (name "Looping")))
				(method (mod-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-type-anno (name "describe")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Looping")))))
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
a.Looping :  where [a.Looping, a.to_str : a -> Str]

describe : a -> Str where [a.Looping]
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
			(e-runtime-error (tag "erroneous_value_expr")))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Looping") (local))))))
	(s-where-alias-decl
		(ty-header (name "Looping"))
		(ty-rigid-var (name "a"))
		(where
			(alias
				(ty-rigid-var-lookup (ty-rigid-var (name "a")))
				(ty-lookup (name "Looping") (local)))
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-lookup (name "Str") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str")))
	(type_decls
		(where-alias (type "Error")
			(ty-header (name "Looping"))))
	(expressions
		(expr (type "a -> Str"))))
~~~
