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
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-if
				(if-branches
					(if-branch
						(e-dispatch-call (method "is_lte") (constraint-fn-var 63)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "1"))))
						(e-lookup-local
							(p-assign (ident "n")))))
				(if-else
					(e-dispatch-call (method "plus") (constraint-fn-var 144)
						(receiver
							(e-call (constraint-fn-var 105)
								(e-lookup-local
									(p-assign (ident "fib")))
								(e-dispatch-call (method "minus") (constraint-fn-var 101)
									(receiver
										(e-lookup-local
											(p-assign (ident "n"))))
									(args
										(e-num (value "1"))))))
						(args
							(e-call (constraint-fn-var 143)
								(e-lookup-local
									(p-assign (ident "fib")))
								(e-dispatch-call (method "minus") (constraint-fn-var 139)
									(receiver
										(e-lookup-local
											(p-assign (ident "n"))))
									(args
										(e-num (value "2"))))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_lte : a, a -> Bool, a.minus : a, b -> a, a.plus : a, a -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_lte : a, a -> Bool, a.minus : a, b -> a, a.plus : a, a -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))))
~~~
