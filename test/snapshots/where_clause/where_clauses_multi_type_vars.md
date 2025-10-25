# META
~~~ini
description=Multiple where constraints on different type variables
type=snippet
~~~
# SOURCE
~~~roc
process : a, b -> c where [a.convert : a -> c, b.transform : b -> c]
process = |_, _| ...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,Comma,Underscore,OpBar,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "process")
			(ty-fn
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty-var (raw "c")))
			(where
				(method (module-of "a") (name "convert")
					(args
						(ty-var (raw "a")))
					(ty-var (raw "c")))
				(method (module-of "b") (name "transform")
					(args
						(ty-var (raw "b")))
					(ty-var (raw "c")))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-underscore)
					(p-underscore))
				(e-ellipsis)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-underscore)
				(p-underscore))
			(e-not-implemented))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))
				(ty-rigid-var (name "c")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "c"))))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "b"))) (name "transform")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
					(ty-rigid-var-lookup (ty-rigid-var (name "c"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, b -> c where [a.convert : a -> cb.transform : b -> c]")))
	(expressions
		(expr (type "a, b -> c where [a.convert : a -> cb.transform : b -> c]"))))
~~~
