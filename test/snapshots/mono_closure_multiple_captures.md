# META
~~~ini
description=Mono test: closure with multiple captured variables
type=mono
~~~
# SOURCE
~~~roc
func = |a, b| {
    add_ab = |x| a + b + x
    add_ab(10)
}
result = func(1, 2)
~~~
# MONO
~~~roc
func : Dec, Dec -> Dec
func = |a, b| {
	add_ab = Closure_add_ab_1({ a: a, b: b })
	match add_ab {
		Closure_add_ab_1({ a, b }) => {
			x = 10
			a + b + x
		}
	}
}

result : Dec
result = func(1, 2)
~~~
# FORMATTED
~~~roc
func = |a, b| {
	add_ab = |x| a + b + x
	add_ab(10)
}
result = func(1, 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
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
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_ab"))
							(e-lambda
								(args
									(p-ident (raw "x")))
								(e-binop (op "+")
									(e-binop (op "+")
										(e-ident (raw "a"))
										(e-ident (raw "b")))
									(e-ident (raw "x")))))
						(e-apply
							(e-ident (raw "add_ab"))
							(e-int (raw "10")))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-block
				(s-let
					(p-assign (ident "add_ab"))
					(e-tag (name "Closure_add_ab_1")
						(args
							(e-record
								(fields
									(field (name "a")
										(e-lookup-local
											(p-assign (ident "a"))))
									(field (name "b")
										(e-lookup-local
											(p-assign (ident "b")))))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "add_ab"))))
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
											(e-binop (op "add")
												(e-lookup-local
													(p-assign (ident "a")))
												(e-lookup-local
													(p-assign (ident "b"))))
											(e-lookup-local
												(p-assign (ident "x")))))))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "1"))
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_c where [_d.from_numeral : Numeral -> _ret]")))
	(expressions
		(expr (type "c, c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c where [c.from_numeral : Numeral -> c]"))))
~~~
