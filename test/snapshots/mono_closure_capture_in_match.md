# META
~~~ini
description=Mono test: closure with capture defined inside match branch
type=mono
~~~
# SOURCE
~~~roc
func = |x| {
	result = match x == 42 {
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
func = |x| {
	result = match x == 42 {
		True => {
			add_x = |y| x + y
			add_x(10)
		}
		False => 0
	}
	result
}

answer : Dec
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
LowerIdent,OpAssign,KwMatch,LowerIdent,OpEquals,Int,OpenCurly,
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
								(e-binop (op "==")
									(e-ident (raw "x"))
									(e-int (raw "42")))
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
								(e-method-eq (negated "false")
									(lhs
										(e-lookup-local
											(p-assign (ident "x"))))
									(rhs
										(e-num (value "42")))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-applied-tag)))
									(value
										(e-block
											(s-let
												(p-assign (ident "add_x"))
												(e-closure
													(captures
														(capture (ident "x")))
													(e-lambda
														(args
															(p-assign (ident "y")))
														(e-dispatch-call (method "plus") (constraint-fn-var 83)
															(receiver
																(e-lookup-local
																	(p-assign (ident "x"))))
															(args
																(e-lookup-local
																	(p-assign (ident "y"))))))))
											(e-call (constraint-fn-var 120)
												(e-lookup-local
													(p-assign (ident "add_x")))
												(e-num (value "10"))))))
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
		(e-call (constraint-fn-var 270)
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "42")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool, a.plus : a, b -> a]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "a -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.is_eq : a, a -> Bool, a.plus : a, b -> a]"))
		(expr (type "Dec"))))
~~~
