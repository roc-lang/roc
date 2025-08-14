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
Evaluation error: error.NotImplemented
---
6
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-call @1.1-1.1
	(e-lambda @1.1-1.1
		(args
			(p-assign @1.1-1.1 (ident "x")))
		(e-binop @1.1-1.1 (op "add")
			(e-lookup-local @1.1-1.1
				(p-assign @1.1-1.1 (ident "x")))
			(e-int @1.1-1.1 (value "1"))))
	(e-int @1.1-1.1 (value "5")))
(e-call @1.2-1.12
	(e-lambda @1.3-1.8
		(args
			(p-assign @1.4-1.5 (ident "x")))
		(e-lookup-local @1.7-1.8
			(p-assign @1.4-1.5 (ident "x"))))
	(e-int @1.10-1.11 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.12 (type "Num(_size)"))
~~~
