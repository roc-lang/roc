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
func : Dec -> Dec
func = |offset| {
	condition = True
	f = if (condition) Closure_f_1({ offset: offset }) else |x| x * 2
	match f {
		Closure_f_1({ offset }) => {
			x = 10
			x + offset
		}
	}
}

result : Dec
result = func(1)
~~~
# FORMATTED
~~~roc
func = |offset| {
	condition = True
	f = if condition |x| x + offset else |x| x * 2
	f(10)
}
result = func(1)
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
								(e-tag (name "Closure_f_1")
									(args
										(e-record
											(fields
												(field (name "offset")
													(e-lookup-local
														(p-assign (ident "offset"))))))))))
						(if-else
							(e-lambda
								(args
									(p-assign (ident "x")))
								(e-binop (op "mul")
									(e-lookup-local
										(p-assign (ident "x")))
									(e-num (value "2")))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "f"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-block
										(s-let
											(p-assign (ident "x"))
											(e-num (value "10")))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "x")))
											(e-lookup-local
												(p-assign (ident "offset")))))))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> a where [_b.from_numeral : _c, _d.from_numeral : Numeral -> Try(_e, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "_arg -> a where [_b.from_numeral : _c, _d.from_numeral : Numeral -> Try(_e, [InvalidNumeral(Str)]), a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "_a where [_b.from_numeral : Numeral -> Try(_c, [InvalidNumeral(Str)])]"))))
~~~
