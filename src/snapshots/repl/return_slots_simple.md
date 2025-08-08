# META
~~~ini
description=Simple test for return slots debugging
type=repl
~~~
# SOURCE
~~~roc
» (|x| x)(5)
» (|x| x + 1)(5)
~~~
# OUTPUT
Evaluation error: error.TypeMismatch
---
Evaluation error: error.TypeMismatch
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-call @1.2-1.12
	(e-lambda @1.3-1.8
		(args
			(p-assign @1.4-1.5 (ident "x")))
		(e-lookup-local @1.7-1.8
			(p-assign @1.4-1.5 (ident "x"))))
	(e-int @1.10-1.11 (value "5")))
---
(e-call @1.2-1.16
	(e-lambda @1.3-1.12
		(args
			(p-assign @1.4-1.5 (ident "x")))
		(e-binop @1.7-1.12 (op "add")
			(e-lookup-local @1.7-1.8
				(p-assign @1.4-1.5 (ident "x")))
			(e-int @1.11-1.12 (value "1"))))
	(e-int @1.14-1.15 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.2-1.12 (type "Num(_size)"))
---
(expr @1.2-1.16 (type "Num(_size)"))
~~~
