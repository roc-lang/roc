# META
~~~ini
description=Nested for loops
type=snippet
~~~
# SOURCE
~~~roc
product : U64
product = {
	var result_ = 0
	for i in [1, 2, 3] {
		for j in [10, 20] {
			result_ = result_ + (i * j)
		}
	}
	result_
}

expect product == 180
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,OpenRound,LowerIdent,OpStar,LowerIdent,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "product")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "product"))
			(e-block
				(statements
					(s-var (name "result_")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "i"))
						(e-list
							(e-int (raw "1"))
							(e-int (raw "2"))
							(e-int (raw "3")))
						(e-block
							(statements
								(s-for
									(p-ident (raw "j"))
									(e-list
										(e-int (raw "10"))
										(e-int (raw "20")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "result_"))
												(e-binop (op "+")
													(e-ident (raw "result_"))
													(e-tuple
														(e-binop (op "*")
															(e-ident (raw "i"))
															(e-ident (raw "j"))))))))))))
					(e-ident (raw "result_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "product"))
				(e-int (raw "180"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "product"))
		(e-block
			(s-var
				(p-assign (ident "result_"))
				(e-num (value "0")))
			(s-expr
				(e-block
					(s-var
						(p-assign (ident "#for_iter_1"))
						(e-dispatch-call (method "iter") (constraint-fn-var 162)
							(receiver
								(e-list
									(elems
										(e-num (value "1"))
										(e-num (value "2"))
										(e-num (value "3")))))
							(args)))
					(s-while
						(e-tag (name "True"))
						(e-match
							(match
								(cond
									(e-dispatch-call (method "next") (constraint-fn-var 217)
										(receiver
											(e-lookup-local
												(p-assign (ident "#for_iter_1"))))
										(args)))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(s-reassign
													(p-assign (ident "#for_iter_1"))
													(e-lookup-local
														(p-assign (ident "#for_rest_2"))))
												(s-expr
													(e-block
														(s-expr
															(e-block
																(s-var
																	(p-assign (ident "#for_iter_3"))
																	(e-dispatch-call (method "iter") (constraint-fn-var 282)
																		(receiver
																			(e-list
																				(elems
																					(e-num (value "10"))
																					(e-num (value "20")))))
																		(args)))
																(s-while
																	(e-tag (name "True"))
																	(e-match
																		(match
																			(cond
																				(e-dispatch-call (method "next") (constraint-fn-var 337)
																					(receiver
																						(e-lookup-local
																							(p-assign (ident "#for_iter_3"))))
																					(args)))
																			(branches
																				(branch
																					(patterns
																						(pattern (degenerate false)
																							(p-applied-tag)))
																					(value
																						(e-block
																							(s-reassign
																								(p-assign (ident "#for_iter_3"))
																								(e-lookup-local
																									(p-assign (ident "#for_rest_4"))))
																							(s-expr
																								(e-block
																									(s-reassign
																										(p-assign (ident "result_"))
																										(e-binop (op "add")
																											(e-lookup-local
																												(p-assign (ident "result_")))
																											(e-binop (op "mul")
																												(e-lookup-local
																													(p-assign (ident "i")))
																												(e-lookup-local
																													(p-assign (ident "j"))))))
																									(e-empty_record)))
																							(e-empty_record))))
																				(branch
																					(patterns
																						(pattern (degenerate false)
																							(p-applied-tag)))
																					(value
																						(e-block
																							(s-break)
																							(e-empty_record))))
																				(branch
																					(patterns
																						(pattern (degenerate false)
																							(p-applied-tag)))
																					(value
																						(e-block
																							(s-break)
																							(e-empty_record))))))))
																(e-empty_record)))
														(e-empty_record)))
												(e-empty_record))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(s-break)
												(e-empty_record))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(s-break)
												(e-empty_record))))))))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "result_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "product"))))
			(rhs
				(e-num (value "180"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
