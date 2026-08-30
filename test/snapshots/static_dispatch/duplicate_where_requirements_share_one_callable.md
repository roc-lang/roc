# META
~~~ini
description=Repeated same-name where requirements describe one method, so they must agree on one callable type; conflicting duplicates are rejected.
type=file
~~~
# SOURCE
~~~roc
process : a -> U64 where [a.convert : a -> U64, a.convert : a -> Str]
process = |x| x.convert()
~~~
# EXPECTED
TYPE MISMATCH - duplicate_where_requirements_share_one_callable.md:1:49:1:69
# PROBLEMS
── ✗ type mismatch ───── duplicate_where_requirements_share_one_callable.md:1:49

This expression is used in an unexpected way.

process : a -> U64 where [a.convert : a -> U64, a.convert : a -> Str]
                                                ^^^^^^^^^^^^^^^^^^^^

It has the type:

    a -> Str where [a.convert : a -> U64]

But you are trying to use it as:

    a -> U64 where [a.convert : a -> U64]

# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,Comma,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "process")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "U64")))
			(where
				(method (mod-of "a") (name "convert")
					(args
						(ty-var (raw "a")))
					(ty (name "U64")))
				(method (mod-of "a") (name "convert")
					(args
						(ty-var (raw "a")))
					(ty (name "Str")))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-method-call (method ".convert")
					(receiver
						(e-ident (raw "x")))
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
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-dispatch-call (method "convert") (constraint-fn-var 240)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args)))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "U64") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "U64") (builtin)))
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "convert")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "Str") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> U64 where [a.convert : a -> U64]")))
	(expressions
		(expr (type "a -> U64 where [a.convert : a -> U64]"))))
~~~
