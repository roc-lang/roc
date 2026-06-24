# META
~~~ini
description=Local self-recursion and backward references are allowed (sequential scoping)
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
|n| {
    fac = |x| if (x <= 1) 1 else x * fac(x - 1)
    helper = |y| fac(y)
    helper(n)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwIf,OpenRound,LowerIdent,OpLessThanOrEq,Int,CloseRound,Int,KwElse,LowerIdent,OpStar,LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-ident (raw "n")))
	(e-block
		(statements
			(s-decl
				(p-ident (raw "fac"))
				(e-lambda
					(args
						(p-ident (raw "x")))
					(e-if-then-else
						(e-tuple
							(e-binop (op "<=")
								(e-ident (raw "x"))
								(e-int (raw "1"))))
						(e-int (raw "1"))
						(e-binop (op "*")
							(e-ident (raw "x"))
							(e-apply
								(e-ident (raw "fac"))
								(e-binop (op "-")
									(e-ident (raw "x"))
									(e-int (raw "1"))))))))
			(s-decl
				(p-ident (raw "helper"))
				(e-lambda
					(args
						(p-ident (raw "y")))
					(e-apply
						(e-ident (raw "fac"))
						(e-ident (raw "y")))))
			(e-apply
				(e-ident (raw "helper"))
				(e-ident (raw "n"))))))
~~~
# FORMATTED
~~~roc
|n| {
	fac = |x| if (x <= 1) 1 else x * fac(x - 1)
	helper = |y| fac(y)
	helper(n)
}
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-assign (ident "n")))
	(e-block
		(s-let
			(p-assign (ident "fac"))
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-if
					(if-branches
						(if-branch
							(e-dispatch-call (method "is_lte") (constraint-fn-var 72)
								(receiver
									(e-lookup-local
										(p-assign (ident "x"))))
								(args
									(e-num (value "1"))))
							(e-num (value "1"))))
					(if-else
						(e-dispatch-call (method "times") (constraint-fn-var 148)
							(receiver
								(e-lookup-local
									(p-assign (ident "x"))))
							(args
								(e-call (constraint-fn-var 147)
									(e-lookup-local
										(p-assign (ident "fac")))
									(e-dispatch-call (method "minus") (constraint-fn-var 143)
										(receiver
											(e-lookup-local
												(p-assign (ident "x"))))
										(args
											(e-num (value "1")))))))))))
		(s-let
			(p-assign (ident "helper"))
			(e-lambda
				(args
					(p-assign (ident "y")))
				(e-call (constraint-fn-var 162)
					(e-lookup-local
						(p-assign (ident "fac")))
					(e-lookup-local
						(p-assign (ident "y"))))))
		(e-call (constraint-fn-var 175)
			(e-lookup-local
				(p-assign (ident "helper")))
			(e-lookup-local
				(p-assign (ident "n"))))))
~~~
# TYPES
~~~clojure
(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_lte : a, a -> Bool, a.minus : a, b -> a, a.times : a, a -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
