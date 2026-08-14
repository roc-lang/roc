# META
~~~ini
description=Applying a where alias to the wrong number of arguments is rejected
type=snippet
~~~
# SOURCE
~~~roc
a.Encodable(fmt) : where [a.encode : fmt -> fmt]

encode_once : a -> Str where [a.Encodable]
encode_once = |value| value.encode("")
~~~
# EXPECTED
TOO FEW ARGS - where_alias_arity_mismatch.md:3:31:3:42
# PROBLEMS
── ✗ too few args ─────────────────────────── where_alias_arity_mismatch.md:3:31

The type Encodable expects 1 argument, but got 0 instead.

encode_once : a -> Str where [a.Encodable]
                              ^^^^^^^^^^^

# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name ".Encodable")
				(args
					(ty-var (raw "fmt"))))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "encode")
					(args
						(ty-var (raw "fmt")))
					(ty-var (raw "fmt")))))
		(s-type-anno (name "encode_once")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty (name "Encodable")))))
		(s-decl
			(p-ident (raw "encode_once"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-method-call (method ".encode")
					(receiver
						(e-ident (raw "value")))
					(args
						(e-string
							(e-string-part (raw "")))))))))
~~~
# FORMATTED
~~~roc
a.Encodable(fmt) :  where [a.encode : fmt -> fmt]

encode_once : a -> Str where [a.Encodable]
encode_once = |value| value.encode("")
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "encode_once"))
		(e-runtime-error (tag "erroneous_value_expr"))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-lookup (name "Encodable") (local))))))
	(s-where-alias-decl
		(ty-header (name "Encodable")
			(ty-args
				(ty-rigid-var (name "fmt"))))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "encode")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "fmt")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error -> Str")))
	(type_decls
		(where-alias (type "a where [a.encode : fmt -> fmt]")
			(ty-header (name "Encodable")
				(ty-args
					(ty-rigid-var (name "fmt"))))))
	(expressions
		(expr (type "Error -> Str"))))
~~~
