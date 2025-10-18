# META
~~~ini
description=Lambda currying with polymorphic function constraints - tests if numeric literals in curried functions get properly constrained
type=snippet
~~~
# SOURCE
~~~roc
# Function that returns a function with polymorphic type
makeAdder : a -> (a -> a)
makeAdder = |x| |y| x + y

# Should constrain the literal 5 to I64
curriedAdd : I64 -> I64
curriedAdd = makeAdder(5)

# Higher-order function that applies a function twice
applyTwice : (a -> a), a -> a
applyTwice = |f, x| f(f(x))

# Should constrain the literal 3 to I64
addThreeTwice : I64 -> I64
addThreeTwice = |n| applyTwice(|x| x + 3, n)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpColon,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,Comma,LowerIdent,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "makeAdder")
			(ty-fn
				(ty-var (raw "a"))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "a")))))
		(s-decl
			(p-ident (raw "makeAdder"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-lambda
					(args
						(p-ident (raw "y")))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))
		(s-type-anno (name "curriedAdd")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "curriedAdd"))
			(e-apply
				(e-ident (raw "makeAdder"))
				(e-int (raw "5"))))
		(s-type-anno (name "applyTwice")
			(ty-fn
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "a")))
				(ty-var (raw "a"))
				(ty-var (raw "a"))))
		(s-decl
			(p-ident (raw "applyTwice"))
			(e-lambda
				(args
					(p-ident (raw "f"))
					(p-ident (raw "x")))
				(e-apply
					(e-ident (raw "f"))
					(e-apply
						(e-ident (raw "f"))
						(e-ident (raw "x"))))))
		(s-type-anno (name "addThreeTwice")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "addThreeTwice"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-apply
					(e-ident (raw "applyTwice"))
					(e-lambda
						(args
							(p-ident (raw "x")))
						(e-binop (op "+")
							(e-ident (raw "x"))
							(e-int (raw "3"))))
					(e-ident (raw "n")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "makeAdder"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-closure
				(captures
					(capture (ident "x")))
				(e-lambda
					(args
						(p-assign (ident "y")))
					(e-binop (op "add")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-rigid-var (name "a"))
					(ty-parens
						(ty-fn (effectful false)
							(ty-rigid-var-lookup (ty-rigid-var (name "a")))
							(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))))
	(d-let
		(p-assign (ident "curriedAdd"))
		(e-call
			(e-lookup-local
				(p-assign (ident "makeAdder")))
			(e-num (value "5")))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin))))))
	(d-let
		(p-assign (ident "applyTwice"))
		(e-lambda
			(args
				(p-assign (ident "f"))
				(p-assign (ident "x")))
			(e-call
				(e-lookup-local
					(p-assign (ident "f")))
				(e-call
					(e-lookup-local
						(p-assign (ident "f")))
					(e-lookup-local
						(p-assign (ident "x"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-parens
						(ty-fn (effectful false)
							(ty-rigid-var (name "a"))
							(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))
					(ty-rigid-var-lookup (ty-rigid-var (name "a")))))))
	(d-let
		(p-assign (ident "addThreeTwice"))
		(e-closure
			(captures
				(capture (ident "applyTwice")))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-call
					(e-lookup-local
						(p-assign (ident "applyTwice")))
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "x")))
							(e-num (value "3"))))
					(e-lookup-local
						(p-assign (ident "n"))))))
		(annotation
			(declared-type
				(ty-fn (effectful false)
					(ty-lookup (name "I64") (builtin))
					(ty-lookup (name "I64") (builtin)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a -> a"))
		(patt (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(patt (type "a -> a, a -> a"))
		(patt (type "Num(Int(Signed64)) -> Num(Int(Signed64))")))
	(expressions
		(expr (type "a -> a -> a"))
		(expr (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))
		(expr (type "a -> a, a -> a"))
		(expr (type "Num(Int(Signed64)) -> Num(Int(Signed64))"))))
~~~
