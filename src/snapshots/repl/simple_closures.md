# META
~~~ini
description=Simple closures
type=repl
~~~
# SOURCE
~~~roc
» (|s| s)("Test")
» (|x| x)(42)
» (|x| !x)(True)
~~~
# OUTPUT
Evaluation error: error.NotImplemented
---
Evaluation error: error.NotImplemented
---
False
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-call @1.1-1.15
	(e-lambda @1.2-1.8
		(args
			(p-assign @1.3-1.4 (ident "x")))
		(e-unary-not @1.6-1.8
			(e-lookup-local @1.7-1.8
				(p-assign @1.3-1.4 (ident "x")))))
	(e-nominal @1.10-1.14 (nominal "Bool")
		(e-tag @1.10-1.14 (name "True"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.15 (type "Bool"))
~~~
