# META
~~~ini
description=Unary minus operation on lambda parameter
type=expr
~~~
# SOURCE
~~~roc
(|x| -x)(5)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpUnaryMinus,LowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
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
				(e-ident (raw "x")))))
	(e-int (raw "5")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call (constraint-fn-var 47)
	(e-lambda
		(args
			(p-assign (ident "x")))
		(e-dispatch-call (method "negate") (constraint-fn-var 12)
			(receiver
				(e-lookup-local
					(p-assign (ident "x"))))
			(args)))
	(e-num (value "5")))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
