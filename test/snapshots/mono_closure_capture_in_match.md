# META
~~~ini
description=Mono test: closure with capture defined inside match branch
type=mono
~~~
# SOURCE
~~~roc
func = |x| {
	result = match True {
		True => {
			add_x = |y| x + y
			add_x(10)
		}
		False => 0
	}
	result
}
answer = func(42)
~~~
# MONO
~~~roc
c1_add_x = |y, captures| captures.x + y

func = |x| {
	result = match True {
		True => {
			add_x = C1_add_x({ x: x })
			match add_x {
				C1_add_x(captures) => c1_add_x(10, captures)
			}
		}
		False => 0
	}
	result
}

answer = func(42)
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
LowerIdent,OpAssign,KwMatch,UpperIdent,OpenCurly,
UpperIdent,OpFatArrow,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
UpperIdent,OpFatArrow,Int,
CloseCurly,
LowerIdent,
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
							(p-ident (raw "result"))
							(e-match
								(e-tag (raw "True"))
								(branches
									(branch
										(p-tag (raw "True"))
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
													(e-int (raw "10"))))))
									(branch
										(p-tag (raw "False"))
										(e-int (raw "0"))))))
						(e-ident (raw "result"))))))
		(s-decl
			(p-ident (raw "answer"))
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
					(p-assign (ident "result"))
					(e-match
						(match
							(cond
								(e-tag (name "True")))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
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
																	(e-num (value "10"))
																	(e-lookup-local
																		(p-assign (ident "captures"))))))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-num (value "0"))))))))
				(e-lookup-local
					(p-assign (ident "result"))))))
	(d-let
		(p-assign (ident "answer"))
		(e-call
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "42")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [_d.from_numeral : a -> b, a.plus : a, c -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, _arg -> b]"))
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "a -> b where [_c.from_numeral : a -> b, a.plus : a, _arg -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, _arg2 -> b]"))
		(expr (type "_a where [_b.plus : c, d -> c, _e.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.from_numeral : f -> c, f.plus : f, g -> f]"))))
~~~
