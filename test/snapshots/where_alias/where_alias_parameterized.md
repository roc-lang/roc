# META
~~~ini
description=A where alias can take parameters, which its constraints may mention
type=snippet
~~~
# SOURCE
~~~roc
a.Encodable(fmt) : where [a.encode : a, fmt -> fmt]

encode_twice : a, fmt -> fmt where [a.Encodable(fmt)]
encode_twice = |value, fmt| value.encode(value.encode(fmt))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
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
		(s-type-anno (name "encode_twice")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "fmt"))
				(ty-var (raw "fmt")))
			(where
				(alias (mod-of "a")
					(ty-apply
						(ty (name "Encodable"))
						(ty-var (raw "fmt"))))))
		(s-decl
			(p-ident (raw "encode_twice"))
			(e-lambda
				(args
					(p-ident (raw "value"))
					(p-ident (raw "fmt")))
				(e-method-call (method ".encode")
					(receiver
						(e-ident (raw "value")))
					(args
						(e-method-call (method ".encode")
							(receiver
								(e-ident (raw "value")))
							(args
								(e-ident (raw "fmt"))))))))))
~~~
# FORMATTED
~~~roc
a.Encodable(fmt) :  where [a.encode : a, fmt -> fmt]

encode_twice : a, fmt -> fmt where [a.Encodable(fmt)]
encode_twice = |value, fmt| value.encode(value.encode(fmt))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "encode_twice"))
		(e-lambda
			(args
				(p-assign (ident "value"))
				(p-assign (ident "fmt")))
			(e-dispatch-call (method "encode") (constraint-fn-var 243)
				(receiver
					(e-lookup-local
						(p-assign (ident "value"))))
				(args
					(e-dispatch-call (method "encode") (constraint-fn-var 241)
						(receiver
							(e-lookup-local
								(p-assign (ident "value"))))
						(args
							(e-lookup-local
								(p-assign (ident "fmt"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "fmt"))
				(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))
			(where
				(alias
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-apply (name "Encodable") (local)
						(ty-rigid-var-lookup (ty-rigid-var (name "fmt"))))))))
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
		(patt (type "a, fmt -> fmt where [a.encode : a, fmt -> fmt]")))
	(type_decls
		(where-alias (type "a where [a.encode : a, fmt -> fmt]")
			(ty-header (name "Encodable")
				(ty-args
					(ty-rigid-var (name "fmt"))))))
	(expressions
		(expr (type "a, fmt -> fmt where [a.encode : a, fmt -> fmt]"))))
~~~
