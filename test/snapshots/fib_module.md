# META
~~~ini
description=Fibonacci fn
type=snippet
~~~
# SOURCE
~~~roc
fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpLessThanOrEq,Int,LowerIdent,KwElse,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,OpPlus,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "fib"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-if-then-else
					(e-binop (op "<=")
						(e-ident (raw "n"))
						(e-int (raw "1")))
					(e-ident (raw "n"))
					(e-binop (op "+")
						(e-apply
							(e-ident (raw "fib"))
							(e-binop (op "-")
								(e-ident (raw "n"))
								(e-int (raw "1"))))
						(e-apply
							(e-ident (raw "fib"))
							(e-binop (op "-")
								(e-ident (raw "n"))
								(e-int (raw "2"))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "fib"))
		(e-closure
			(captures
				(capture (ident "fib")))
			(e-lambda
				(args
					(p-assign (ident "n")))
				(e-if
					(if-branches
						(if-branch
							(e-binop (op "le")
								(e-lookup-local
									(p-assign (ident "n")))
								(e-num (value "1")))
							(e-lookup-local
								(p-assign (ident "n")))))
					(if-else
						(e-binop (op "add")
							(e-call
								(e-lookup-local
									(p-assign (ident "fib")))
								(e-binop (op "sub")
									(e-lookup-local
										(p-assign (ident "n")))
									(e-num (value "1"))))
							(e-call
								(e-lookup-local
									(p-assign (ident "fib")))
								(e-binop (op "sub")
									(e-lookup-local
										(p-assign (ident "n")))
									(e-num (value "2")))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size) -> Num(_size2)")))
	(expressions
		(expr (type "Num(_size) -> Num(_size2)"))))
~~~
