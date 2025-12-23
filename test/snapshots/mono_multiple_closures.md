# META
~~~ini
description=Mono test: multiple different closures at top-level
type=mono
~~~
# SOURCE
~~~roc
x = 10
y = 20
add_x = |a| a + x
add_y = |b| b + y
result1 = add_x(5)
result2 = add_y(5)
~~~
# MONO
~~~roc
x : Dec
x = 10
y : Dec
y = 20
add_x : Dec -> Dec
add_x = Closure_add_x_1({ x: x })
add_y : Dec -> Dec
add_y = Closure_add_y_2({ y: y })
result1 : Dec
result1 = match add_x {
	Closure_add_x_1({ x }) => {
		a = 5
		a + x
	}
}
result2 : Dec
result2 = match add_y {
	Closure_add_y_2({ y }) => {
		b = 5
		b + y
	}
}
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
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "10")))
		(s-decl
			(p-ident (raw "y"))
			(e-int (raw "20")))
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
		(s-decl
			(p-ident (raw "result1"))
			(e-apply
				(e-ident (raw "add_x"))
				(e-int (raw "5"))))
		(s-decl
			(p-ident (raw "result2"))
			(e-apply
				(e-ident (raw "add_y"))
				(e-int (raw "5"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "x"))
		(e-num (value "10")))
	(d-let
		(p-assign (ident "y"))
		(e-num (value "20")))
	(d-let
		(p-assign (ident "add_x"))
		(e-tag (name "Closure_add_x_1")
			(args
				(e-record
					(fields
						(field (name "x")
							(e-lookup-local
								(p-assign (ident "x")))))))))
	(d-let
		(p-assign (ident "add_y"))
		(e-tag (name "Closure_add_y_2")
			(args
				(e-record
					(fields
						(field (name "y")
							(e-lookup-local
								(p-assign (ident "y")))))))))
	(d-let
		(p-assign (ident "result1"))
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
										(p-assign (ident "x")))))))))))
	(d-let
		(p-assign (ident "result2"))
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
										(p-assign (ident "y"))))))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "Str"))
		(expr (type "[]"))
		(expr (type "[Error]"))
		(expr (type "Numeral"))))
~~~
