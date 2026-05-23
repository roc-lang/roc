# META
~~~ini
description=Mono test: dispatch for closure call with captured variable
type=mono
~~~
# SOURCE
~~~roc
func = |offset| {
	condition = True
	f = if condition |x| x + offset else |x| x * 2
	f(10)
}

result = func(1)
~~~
# MONO
~~~roc
func = |offset| {
	condition = True
	f = if (condition) |x| x.plus(offset) else |x| x.times(2)
	f(10)
}

result : Dec
result = func(1)
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,KwIf,LowerIdent,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,KwElse,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "func"))
			(e-lambda
				(args
					(p-ident (raw "offset")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "condition"))
							(e-tag (raw "True")))
						(s-decl
							(p-ident (raw "f"))
							(e-if-then-else
								(e-ident (raw "condition"))
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "+")
										(e-ident (raw "x"))
										(e-ident (raw "offset"))))
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "*")
										(e-ident (raw "x"))
										(e-int (raw "2"))))))
						(e-apply
							(e-ident (raw "f"))
							(e-int (raw "10")))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "1"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "offset")))
			(e-block
				(s-let
					(p-assign (ident "condition"))
					(e-tag (name "True")))
				(s-let
					(p-assign (ident "f"))
					(e-if
						(if-branches
							(if-branch
								(e-lookup-local
									(p-assign (ident "condition")))
								(e-closure
									(captures
										(capture (ident "offset")))
									(e-lambda
										(args
											(p-assign (ident "x")))
										(e-dispatch-call (method "plus") (constraint-fn-var 44)
											(receiver
												(e-lookup-local
													(p-assign (ident "x"))))
											(args
												(e-lookup-local
													(p-assign (ident "offset")))))))))
						(if-else
							(e-lambda
								(args
									(p-assign (ident "x")))
								(e-dispatch-call (method "times") (constraint-fn-var 56)
									(receiver
										(e-lookup-local
											(p-assign (ident "x"))))
									(args
										(e-num (value "2"))))))))
				(e-call (constraint-fn-var 76)
					(e-lookup-local
						(p-assign (ident "f")))
					(e-num (value "10"))))))
	(d-let
		(p-assign (ident "result"))
		(e-call (constraint-fn-var 99)
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, a -> b, b.times : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "a -> b where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, a -> b, b.times : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "Dec"))))
~~~
