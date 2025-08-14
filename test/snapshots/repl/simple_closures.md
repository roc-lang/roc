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
(e-call @1.1-1.1
	(e-lambda @1.1-1.1
		(args
			(p-assign @1.1-1.1 (ident "x")))
		(e-unary-not @1.1-1.1
			(e-lookup-local @1.1-1.1
				(p-assign @1.1-1.1 (ident "x")))))
	(e-nominal @1.1-1.1 (nominal "Bool")
		(e-tag @1.1-1.1 (name "True"))))
(e-call @1.2-1.17
	(e-lambda @1.3-1.8
		(args
			(p-assign @1.4-1.5 (ident "s")))
		(e-lookup-local @1.7-1.8
			(p-assign @1.4-1.5 (ident "s"))))
	(e-string @1.10-1.16
		(e-literal @1.11-1.15 (string "Test"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Bool"))
(expr @1.2-1.17 (type "Str"))
~~~
