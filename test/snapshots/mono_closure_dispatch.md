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
closure_f_1 = |x, captures| x + captures.offset

closure_f_2 = |x| x * 2

func : Dec -> []
func = |offset| {
	condition = True
	f = if (condition) Closure_f_1({ offset: offset }) else Closure_f_2({})
	match f {
		Closure_f_1(captures) => closure_f_1(10, captures)
		Closure_f_2({}) => closure_f_2(10)
	}
}

result : []
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
							(e-tag (name "Closure_f_2")
								(args
									(e-empty_record))))))
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
									(e-call
										(e-lookup-local
											(p-assign (ident "closure_f_1")))
										(e-num (value "10"))
										(e-lookup-local
											(p-assign (ident "captures"))))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-call
										(e-lookup-local
											(p-assign (ident "closure_f_2")))
										(e-num (value "10")))))))))))
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
		(patt (type "a -> b where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral([])]), b.from_numeral : Numeral -> Try(b, a -> [])]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral([])])]")))
	(expressions
		(expr (type "a -> [] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral([])])]"))
		(expr (type "[]"))))
~~~
