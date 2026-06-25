# META
~~~ini
description=Binop omnibus - singleline - no spaces
type=expr
~~~
# SOURCE
~~~roc
Err(foo)??12>5*5 or 13+2<5 and 10-1>=16 or 12<=3/5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpDoubleQuestion,Int,OpGreaterThan,Int,OpStar,Int,OpOr,Int,OpPlus,Int,OpLessThan,Int,OpAnd,Int,OpBinaryMinus,Int,OpGreaterThanOrEq,Int,OpOr,Int,OpLessThanOrEq,Int,OpSlash,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "or")
	(e-binop (op ">")
		(e-binop (op "??")
			(e-apply
				(e-tag (raw "Err"))
				(e-ident (raw "foo")))
			(e-int (raw "12")))
		(e-binop (op "*")
			(e-int (raw "5"))
			(e-int (raw "5"))))
	(e-binop (op "or")
		(e-binop (op "and")
			(e-binop (op "<")
				(e-binop (op "+")
					(e-int (raw "13"))
					(e-int (raw "2")))
				(e-int (raw "5")))
			(e-binop (op ">=")
				(e-binop (op "-")
					(e-int (raw "10"))
					(e-int (raw "1")))
				(e-int (raw "16"))))
		(e-binop (op "<=")
			(e-int (raw "12"))
			(e-binop (op "/")
				(e-int (raw "3"))
				(e-int (raw "5"))))))
~~~
# FORMATTED
~~~roc
Err(foo) ?? 12 > 5 * 5 or 13 + 2 < 5 and 10 - 1 >= 16 or 12 <= 3 / 5
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-dispatch-call (method "is_gt") (constraint-fn-var 178)
				(receiver
					(e-match
						(match
							(cond
								(e-tag (name "Err")
									(args
										(e-runtime-error (tag "ident_not_in_scope")))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-lookup-local
											(p-assign (ident "#ok")))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal-external (builtin)
												(p-applied-tag))))
									(value
										(e-num (value "12"))))))))
				(args
					(e-dispatch-call (method "times") (constraint-fn-var 173)
						(receiver
							(e-num (value "5")))
						(args
							(e-num (value "5"))))))
			(e-nominal-external
				(builtin)
				(e-tag (name "True")))))
	(if-else
		(e-if
			(if-branches
				(if-branch
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_lt") (constraint-fn-var 295)
									(receiver
										(e-dispatch-call (method "plus") (constraint-fn-var 257)
											(receiver
												(e-num (value "13")))
											(args
												(e-num (value "2")))))
									(args
										(e-num (value "5"))))
								(e-dispatch-call (method "is_gte") (constraint-fn-var 404)
									(receiver
										(e-dispatch-call (method "minus") (constraint-fn-var 366)
											(receiver
												(e-num (value "10")))
											(args
												(e-num (value "1")))))
									(args
										(e-num (value "16"))))))
						(if-else
							(e-nominal-external
								(builtin)
								(e-tag (name "False")))))
					(e-nominal-external
						(builtin)
						(e-tag (name "True")))))
			(if-else
				(e-dispatch-call (method "is_lte") (constraint-fn-var 523)
					(receiver
						(e-num (value "12")))
					(args
						(e-dispatch-call (method "div_by") (constraint-fn-var 518)
							(receiver
								(e-num (value "3")))
							(args
								(e-num (value "5"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Bool"))
~~~
