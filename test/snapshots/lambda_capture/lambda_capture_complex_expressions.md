# META
~~~ini
description=Complex expressions with captures - lambda with conditionals and captures
type=expr
~~~
# SOURCE
~~~roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpBar,LowerIdent,OpBar,KwIf,LowerIdent,OpGreaterThan,Int,OpenRound,LowerIdent,OpPlus,LowerIdent,CloseRound,KwElse,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-apply
		(e-tuple
			(e-lambda
				(args
					(p-ident (raw "outer")))
				(e-lambda
					(args
						(p-ident (raw "inner")))
					(e-if-then-else
						(e-binop (op ">")
							(e-ident (raw "outer"))
							(e-int (raw "0")))
						(e-tuple
							(e-binop (op "+")
								(e-ident (raw "outer"))
								(e-ident (raw "inner"))))
						(e-ident (raw "inner"))))))
		(e-int (raw "1")))
	(e-int (raw "-2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 135)
	(e-call (constraint-fn-var 101)
		(e-lambda
			(args
				(p-assign (ident "outer")))
			(e-closure
				(captures
					(capture (ident "outer")))
				(e-lambda
					(args
						(p-assign (ident "inner")))
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_gt") (constraint-fn-var 61)
									(receiver
										(e-lookup-local
											(p-assign (ident "outer"))))
									(args
										(e-num (value "0"))))
								(e-dispatch-call (method "plus") (constraint-fn-var 66)
									(receiver
										(e-lookup-local
											(p-assign (ident "outer"))))
									(args
										(e-lookup-local
											(p-assign (ident "inner")))))))
						(if-else
							(e-lookup-local
								(p-assign (ident "inner"))))))))
		(e-num (value "1")))
	(e-num (value "-2")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
