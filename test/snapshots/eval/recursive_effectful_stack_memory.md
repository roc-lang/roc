# META
~~~ini
description=Test stack memory restoration for recursive effectful functions
type=snippet
~~~
# SOURCE
~~~roc
# Recursive function that exercises stack memory during recursion.
# This tests that stack memory is properly reclaimed when returning from
# recursive closure calls, preventing memory leaks.

factorial : I64 -> I64
factorial = |n|
	match n <= 1 {
		True => 1
		False => n * factorial(n - 1)
	}

result : I64
result = factorial(10)

expect result == 3628800
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
KwMatch,LowerIdent,OpLessThanOrEq,Int,OpenCurly,
UpperIdent,OpFatArrow,Int,
UpperIdent,OpFatArrow,LowerIdent,OpStar,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "factorial")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "factorial"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-match
					(e-binop (op "<=")
						(e-ident (raw "n"))
						(e-int (raw "1")))
					(branches
						(branch
							(p-tag (raw "True"))
							(e-int (raw "1")))
						(branch
							(p-tag (raw "False"))
							(e-binop (op "*")
								(e-ident (raw "n"))
								(e-apply
									(e-ident (raw "factorial"))
									(e-binop (op "-")
										(e-ident (raw "n"))
										(e-int (raw "1"))))))))))
		(s-type-anno (name "result")
			(ty (name "I64")))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "factorial"))
				(e-int (raw "10"))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-int (raw "3628800"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "factorial"))
		(e-closure
			(captures
				(capture (ident "factorial")))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-match
					(match
						(cond
							(e-binop (op "le")
								(e-lookup-local
									(p-assign (ident "n")))
								(e-num (value "1"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-num (value "1"))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-binop (op "mul")
										(e-lookup-local
											(p-assign (ident "n")))
										(e-call
											(e-lookup-local
												(p-assign (ident "factorial")))
											(e-binop (op "sub")
												(e-lookup-local
													(p-assign (ident "n")))
												(e-num (value "1"))))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "factorial")))
			(e-num (value "10")))
		(annotation
			(ty-lookup (name "I64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-num (value "3628800")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> I64"))
		(patt (type "I64")))
	(expressions
		(expr (type "I64 -> I64"))
		(expr (type "I64"))))
~~~
