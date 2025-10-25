# META
~~~ini
description=Simple where clause with single constraint
type=snippet
~~~
# SOURCE
~~~roc
stringify : a -> Str where [a.to_str : a -> Str]
stringify = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "stringify")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "Str")))
			(where
				(method (module-of "a") (name "to_str")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "stringify"))
			(e-lambda
				(args
					(p-ident (raw "value")))
				(e-field-access
					(e-ident (raw "value"))
					(e-apply
						(e-ident (raw "to_str"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "stringify"))
		(e-lambda
			(args
				(p-assign (ident "value")))
			(e-dot-access (field "to_str")
				(receiver
					(e-lookup-local
						(p-assign (ident "value"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "Str") (external-module "Str")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_str")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (external-module "Str")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> Str where [a.to_str : a -> Str]")))
	(expressions
		(expr (type "a -> Str where [a.to_str : a -> Str]"))))
~~~
