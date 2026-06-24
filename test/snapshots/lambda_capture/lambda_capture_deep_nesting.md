# META
~~~ini
description=Deep nesting with multiple captures - five-level nested lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|a| |b| |c| |d| |e| a + b + c + d + e)(1)(2)(3)(4)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-apply
		(e-apply
			(e-apply
				(e-apply
					(e-tuple
						(e-lambda
							(args
								(p-ident (raw "a")))
							(e-lambda
								(args
									(p-ident (raw "b")))
								(e-lambda
									(args
										(p-ident (raw "c")))
									(e-lambda
										(args
											(p-ident (raw "d")))
										(e-lambda
											(args
												(p-ident (raw "e")))
											(e-binop (op "+")
												(e-binop (op "+")
													(e-binop (op "+")
														(e-binop (op "+")
															(e-ident (raw "a"))
															(e-ident (raw "b")))
														(e-ident (raw "c")))
													(e-ident (raw "d")))
												(e-ident (raw "e")))))))))
					(e-int (raw "1")))
				(e-int (raw "2")))
			(e-int (raw "3")))
		(e-int (raw "4")))
	(e-int (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 231)
	(e-call (constraint-fn-var 197)
		(e-call (constraint-fn-var 163)
			(e-call (constraint-fn-var 129)
				(e-call (constraint-fn-var 90)
					(e-lambda
						(args
							(p-assign (ident "a")))
						(e-closure
							(captures
								(capture (ident "a")))
							(e-lambda
								(args
									(p-assign (ident "b")))
								(e-closure
									(captures
										(capture (ident "a"))
										(capture (ident "b")))
									(e-lambda
										(args
											(p-assign (ident "c")))
										(e-closure
											(captures
												(capture (ident "a"))
												(capture (ident "b"))
												(capture (ident "c")))
											(e-lambda
												(args
													(p-assign (ident "d")))
												(e-closure
													(captures
														(capture (ident "a"))
														(capture (ident "b"))
														(capture (ident "c"))
														(capture (ident "d")))
													(e-lambda
														(args
															(p-assign (ident "e")))
														(e-dispatch-call (method "plus") (constraint-fn-var 55)
															(receiver
																(e-dispatch-call (method "plus") (constraint-fn-var 53)
																	(receiver
																		(e-dispatch-call (method "plus") (constraint-fn-var 51)
																			(receiver
																				(e-dispatch-call (method "plus") (constraint-fn-var 49)
																					(receiver
																						(e-lookup-local
																							(p-assign (ident "a"))))
																					(args
																						(e-lookup-local
																							(p-assign (ident "b"))))))
																			(args
																				(e-lookup-local
																					(p-assign (ident "c"))))))
																	(args
																		(e-lookup-local
																			(p-assign (ident "d"))))))
															(args
																(e-lookup-local
																	(p-assign (ident "e"))))))))))))))
					(e-num (value "1")))
				(e-num (value "2")))
			(e-num (value "3")))
		(e-num (value "4")))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
