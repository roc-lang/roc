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
c1_add_x = |a, captures| a + captures.x

c2_add_y = |b, captures| b + captures.y

func = |x, y| {
	add_x = C1_add_x({ x: x })
	add_y = C2_add_y({ y: y })
	match add_x {
		C1_add_x(captures) => c1_add_x(5, captures)
	} + match add_y {
		C2_add_y(captures) => c2_add_y(5, captures)
	}
}

result = func(10, 20)
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
					(e-tag (name "#1_add_x")
						(args
							(e-record
								(fields
									(field (name "x")
										(e-lookup-local
											(p-assign (ident "x")))))))))
				(s-let
					(p-assign (ident "add_y"))
					(e-tag (name "#2_add_y")
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
										(e-call
											(e-lookup-local
												(p-assign (ident "c1_add_x")))
											(e-num (value "5"))
											(e-lookup-local
												(p-assign (ident "captures")))))))))
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
										(e-call
											(e-lookup-local
												(p-assign (ident "c2_add_y")))
											(e-num (value "5"))
											(e-lookup-local
												(p-assign (ident "captures")))))))))))))
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
		(patt (type "_arg, _arg2 -> c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.plus : c, d -> c, d.from_numeral : Numeral -> Try(d, Numeral), d.plus : d, _arg3 -> d]"))
		(patt (type "c where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.plus : c, d -> c, d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)]), d.plus : d, _arg -> d]")))
	(expressions
		(expr (type "_arg, _arg2 -> Numeral"))
		(expr (type "Numeral"))))
~~~
