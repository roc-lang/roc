# META
~~~ini
description=Mono test: closure with single captured variable
type=mono
~~~
# SOURCE
~~~roc
func = |x| {
    add_x = |y| x + y
    add_x(10)
}
result = func(42)
~~~
# MONO
~~~roc
func : Dec -> Dec
func = |x| {
	add_x = Closure_add_x_1({ x: x })
	match add_x {
		Closure_add_x_1({ x }) => {
			y = 10
			x + y
		}
	}
}

result : Dec
result = func(42)
~~~
# FORMATTED
~~~roc
func = |x| {
	add_x = |y| x + y
	add_x(10)
}
result = func(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
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
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_x"))
							(e-lambda
								(args
									(p-ident (raw "y")))
								(e-binop (op "+")
									(e-ident (raw "x"))
									(e-ident (raw "y")))))
						(e-apply
							(e-ident (raw "add_x"))
							(e-int (raw "10")))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "42"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "x")))
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
											(p-assign (ident "y"))
											(e-num (value "10")))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "x")))
											(e-lookup-local
												(p-assign (ident "y")))))))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "42")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "Numeral -> Try(a -> a, [InvalidNumeral(Str)]) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))))
~~~
