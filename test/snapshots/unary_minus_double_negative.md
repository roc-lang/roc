# META
~~~ini
description=Double negative unary minus operation
type=expr
~~~
# SOURCE
~~~roc
(|x| -(-x))(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpUnaryMinus,NoSpaceOpenRound,OpUnaryMinus,LowerIdent,CloseRound,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "x")))
			(unary "-"
				(e-tuple
					(unary "-"
						(e-ident (raw "x")))))))
	(e-int (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 53)
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-dispatch-call (method "negate") (constraint-fn-var 18)
			(receiver
				(e-dispatch-call (method "negate") (constraint-fn-var 16)
					(receiver
						(e-lookup-local
							(p-assign (ident "x"))))
					(args)))
			(args)))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
