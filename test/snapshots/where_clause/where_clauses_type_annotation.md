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
				(e-field-access
					(e-ident (raw "a"))
					(e-apply
						(e-ident (raw "to_b"))))))))
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
			(e-dot-access (field "to_b")
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args)))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(s-type-anno (name "convert")
		(ty-fn (effectful false)
			(ty-rigid-var (name "a"))
			(ty-rigid-var (name "b")))
		(where
			(method (module-of "a") (ident "to_b")
				(args
					(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(ext-decl (ident "a.to_b") (kind "value")))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Error")))
	(expressions
		(expr (type "a -> Error"))))
~~~
