# META
~~~ini
description=Mono test: top-level constants are never captured by lambdas
type=mono
~~~
# SOURCE
~~~roc
one : I64
one = 1
add_one : I64 -> I64
add_one = |x| x + one
result : I64
result = add_one(5)
~~~
# MONO
~~~roc
one : I64
one = 1

add_one : I64 -> I64
add_one = |x| x + one

result : I64
result = 6
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "one")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "one"))
			(e-int (raw "1")))
		(s-type-anno (name "add_one")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "add_one"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "one")))))
		(s-type-anno (name "result")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "add_one"))
				(e-int (raw "5"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "one"))
		(e-num (value "1"))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(d-let
		(p-assign (ident "add_one"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "x")))
				(e-lookup-local
					(p-assign (ident "one")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "6"))
		(annotation
			(ty-lookup (name "I64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64"))
		(patt (type "I64 -> I64"))
		(patt (type "I64")))
	(expressions
		(expr (type "I64"))
		(expr (type "I64 -> I64"))
		(expr (type "I64"))))
~~~
