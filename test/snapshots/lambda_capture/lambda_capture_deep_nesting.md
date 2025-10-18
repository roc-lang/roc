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
(e-call
	(e-call
		(e-call
			(e-call
				(e-call
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
												(capture (ident "c"))
												(capture (ident "a"))
												(capture (ident "b")))
											(e-lambda
												(args
													(p-assign (ident "d")))
												(e-closure
													(captures
														(capture (ident "d"))
														(capture (ident "c"))
														(capture (ident "a"))
														(capture (ident "b")))
													(e-lambda
														(args
															(p-assign (ident "e")))
														(e-binop (op "add")
															(e-binop (op "add")
																(e-binop (op "add")
																	(e-binop (op "add")
																		(e-lookup-local
																			(p-assign (ident "a")))
																		(e-lookup-local
																			(p-assign (ident "b"))))
																	(e-lookup-local
																		(p-assign (ident "c"))))
																(e-lookup-local
																	(p-assign (ident "d"))))
															(e-lookup-local
																(p-assign (ident "e")))))))))))))
					(e-num (value "1")))
				(e-num (value "2")))
			(e-num (value "3")))
		(e-num (value "4")))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
