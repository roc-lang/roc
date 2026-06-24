# META
~~~ini
description=Simple type annotation with where clause
type=snippet
~~~
# SOURCE
~~~roc
convert : a -> b where [a.to_b : a -> b]
convert = |a| a.to_b()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "convert")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b")))
			(where
				(method (module-of "a") (name "to_b")
					(args
						(ty-var (raw "a")))
					(ty-var (raw "b")))))
		(s-decl
			(p-ident (raw "convert"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-method-call (method ".to_b")
					(receiver
						(e-ident (raw "a")))
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
		(p-assign (ident "convert"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "to_b") (constraint-fn-var 27)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_b")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.to_b : a -> b]")))
	(expressions
		(expr (type "a -> b where [a.to_b : a -> b]"))))
~~~
