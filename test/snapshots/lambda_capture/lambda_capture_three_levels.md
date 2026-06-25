# META
~~~ini
description=Three-level nested lambda captures - innermost lambda captures from all outer levels
type=expr
~~~
# SOURCE
~~~roc
(|outer| |middle| |inner| outer + middle + inner)(1)(2)(3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-apply
		(e-apply
			(e-tuple
				(e-lambda
					(args
						(p-ident (raw "outer")))
					(e-lambda
						(args
							(p-ident (raw "middle")))
						(e-lambda
							(args
								(p-ident (raw "inner")))
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "outer"))
									(e-ident (raw "middle")))
								(e-ident (raw "inner")))))))
			(e-int (raw "1")))
		(e-int (raw "2")))
	(e-int (raw "3")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 136)
	(e-call (constraint-fn-var 102)
		(e-call (constraint-fn-var 65)
			(e-lambda
				(args
					(p-assign (ident "outer")))
				(e-closure
					(captures
						(capture (ident "outer")))
					(e-lambda
						(args
							(p-assign (ident "middle")))
						(e-closure
							(captures
								(capture (ident "outer"))
								(capture (ident "middle")))
							(e-lambda
								(args
									(p-assign (ident "inner")))
								(e-dispatch-call (method "plus") (constraint-fn-var 30)
									(receiver
										(e-dispatch-call (method "plus") (constraint-fn-var 28)
											(receiver
												(e-lookup-local
													(p-assign (ident "outer"))))
											(args
												(e-lookup-local
													(p-assign (ident "middle"))))))
									(args
										(e-lookup-local
											(p-assign (ident "inner"))))))))))
			(e-num (value "1")))
		(e-num (value "2")))
	(e-num (value "3")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
