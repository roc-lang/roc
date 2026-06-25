# META
~~~ini
description=Simple addition function with expect statement
type=snippet
~~~
# SOURCE
~~~roc
addU8 : U8, U8 -> U8
addU8 = |a, b| a + b

expect addU8(1, 2) == 3
expect addU8(0, 10) == 10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,OpEquals,Int,
KwExpect,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "addU8")
			(ty-fn
				(ty (name "U8"))
				(ty (name "U8"))
				(ty (name "U8"))))
		(s-decl
			(p-ident (raw "addU8"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "b")))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "addU8"))
					(e-int (raw "1"))
					(e-int (raw "2")))
				(e-int (raw "3"))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "addU8"))
					(e-int (raw "0"))
					(e-int (raw "10")))
				(e-int (raw "10"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "addU8"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-dispatch-call (method "plus") (constraint-fn-var 50)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-lookup-local
						(p-assign (ident "b"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin))
				(ty-lookup (name "U8") (builtin)))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 132)
					(e-lookup-local
						(p-assign (ident "addU8")))
					(e-num (value "1"))
					(e-num (value "2"))))
			(rhs
				(e-num (value "3")))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 464)
					(e-lookup-local
						(p-assign (ident "addU8")))
					(e-num (value "0"))
					(e-num (value "10"))))
			(rhs
				(e-num (value "10"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U8, U8 -> U8")))
	(expressions
		(expr (type "U8, U8 -> U8"))))
~~~
