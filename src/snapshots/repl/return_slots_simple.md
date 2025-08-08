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
(e-call @1.1-1.15
	(e-lambda @1.2-1.11
		(args
			(p-assign @1.3-1.4 (ident "x")))
		(e-binop @1.6-1.11 (op "add")
			(e-lookup-local @1.6-1.7
				(p-assign @1.3-1.4 (ident "x")))
			(e-int @1.10-1.11 (value "1"))))
	(e-int @1.13-1.14 (value "5")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "Num(_size)"))
~~~
