# META
~~~ini
description=Mono test: multiple different closures with captures
type=mono
~~~
# SOURCE
~~~roc
func = |x, y| {
    add_x = |a| a + x
    add_y = |b| b + y
    add_x(5) + add_y(5)
}
result = func(10, 20)
~~~
# MONO
~~~roc
func : Dec, Dec -> Dec
func = |x, y| {
	add_x = Closure_add_x_1({ x: x })
	add_y = Closure_add_y_2({ y: y })
	match add_x {
		Closure_add_x_1({ x }) => {
			a = 5
			a + x
		}
	} + match add_y {
		Closure_add_y_2({ y }) => {
			b = 5
			b + y
		}
	}
}

result : Dec
result = func(10, 20)
~~~
# FORMATTED
~~~roc
func = |x, y| {
	add_x = |a| a + x
	add_y = |b| b + y
	add_x(5) + add_y(5)
}
result = func(10, 20)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpPlus,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_x"))
							(e-lambda
								(args
									(p-ident (raw "a")))
								(e-binop (op "+")
									(e-ident (raw "a"))
									(e-ident (raw "x")))))
						(s-decl
							(p-ident (raw "add_y"))
							(e-lambda
								(args
									(p-ident (raw "b")))
								(e-binop (op "+")
									(e-ident (raw "b"))
									(e-ident (raw "y")))))
						(e-binop (op "+")
							(e-apply
								(e-ident (raw "add_x"))
								(e-int (raw "5")))
							(e-apply
								(e-ident (raw "add_y"))
								(e-int (raw "5"))))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "10"))
				(e-int (raw "20"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-block
				(s-let
					(p-assign (ident "add_x"))
					(e-tag (name "Closure_add_x_1")
						(args
							(e-record
								(fields
									(field (name "x")
										(e-lookup-local
											(p-assign (ident "x")))))))))
				(s-let
					(p-assign (ident "add_y"))
					(e-tag (name "Closure_add_y_2")
						(args
							(e-record
								(fields
									(field (name "y")
										(e-lookup-local
											(p-assign (ident "y")))))))))
				(e-binop (op "add")
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "add_x"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-block
											(s-let
												(p-assign (ident "a"))
												(e-num (value "5")))
											(e-binop (op "add")
												(e-lookup-local
													(p-assign (ident "a")))
												(e-lookup-local
													(p-assign (ident "x"))))))))))
					(e-match
						(match
							(cond
								(e-lookup-local
									(p-assign (ident "add_y"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-block
											(s-let
												(p-assign (ident "b"))
												(e-num (value "5")))
											(e-binop (op "add")
												(e-lookup-local
													(p-assign (ident "b")))
												(e-lookup-local
													(p-assign (ident "y"))))))))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "10"))
			(e-num (value "20")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(c, c -> c)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(d, d -> d)]), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(d, d -> d)])]")))
	(expressions
		(expr (type "c, c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(c, c -> c)])]"))
		(expr (type "_c where [_d.from_numeral : Numeral -> Try(_e, [InvalidNumeral(f, f -> f)]), f.from_numeral : Numeral -> Try(f, [InvalidNumeral(f, f -> f)])]"))))
~~~
