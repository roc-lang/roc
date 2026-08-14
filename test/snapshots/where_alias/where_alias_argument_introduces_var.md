# META
~~~ini
description=A where alias reference's argument can be a type variable the same where clause constrains
type=snippet
~~~
# SOURCE
~~~roc
a.Encodable(fmt) : where [a.encode : a, fmt -> fmt]

render : a, fmt -> Str where [a.Encodable(fmt), fmt.finish : fmt -> Str]
render = |value, fmt| value.encode(fmt).finish()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
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
						(ty-var (raw "a"))
						(ty-var (raw "fmt")))
					(ty-var (raw "fmt")))))
		(s-type-anno (name "render")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "fmt"))
				(ty (name "Str")))
			(where
				(alias (mod-of "a")
					(ty-apply
						(ty (name "Encodable"))
						(ty-var (raw "fmt"))))
				(method (mod-of "fmt") (name "finish")
					(args
						(ty-var (raw "fmt")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "render"))
			(e-lambda
				(args
					(p-ident (raw "value"))
					(p-ident (raw "fmt")))
				(e-method-call (method ".finish")
					(receiver
						(e-method-call (method ".encode")
							(receiver
								(e-ident (raw "value")))
							(args
								(e-ident (raw "fmt")))))
					(args))))))
~~~
# FORMATTED
~~~roc
a.Encodable(fmt) :  where [a.encode : a, fmt -> fmt]

render : a, fmt -> Str where [a.Encodable(fmt), fmt.finish : fmt -> Str]
render = |value, fmt| value.encode(fmt).finish()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "render"))
		(e-lambda
			(args
				(p-assign (ident "value"))
				(p-assign (ident "fmt")))
			(e-dispatch-call (method "finish") (constraint-fn-var 259)
				(receiver
					(e-dispatch-call (method "encode") (constraint-fn-var 257)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-lookup-local
								(p-assign (ident "fmt"))))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "fmt"))
				(ty-lookup (name "Str") (builtin)))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-apply (name "Encodable") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "fmt")))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "fmt"))) (name "finish")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
					(ty-lookup (name "Str") (builtin))))))
	(s-where-alias-decl
		(ty-header (name "Encodable")
			(ty-args
				(ty-rigid-var (name "fmt"))))
		(ty-rigid-var (name "a"))
		(where
			(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "encode")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "fmt")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, fmt -> Str where [a.encode : a, fmt -> fmt, fmt.finish : fmt -> Str]")))
	(type_decls
		(where-alias (type "a where [a.encode : a, fmt -> fmt]")
			(ty-header (name "Encodable")
				(ty-args
					(ty-rigid-var (name "fmt"))))))
	(expressions
		(expr (type "a, fmt -> Str where [a.encode : a, fmt -> fmt, fmt.finish : fmt -> Str]"))))
~~~
