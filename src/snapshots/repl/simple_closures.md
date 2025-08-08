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
(e-call @1.2-1.17
	(e-lambda @1.3-1.8
		(args
			(p-assign @1.4-1.5 (ident "s")))
		(e-lookup-local @1.7-1.8
			(p-assign @1.4-1.5 (ident "s"))))
	(e-string @1.10-1.16
		(e-literal @1.11-1.15 (string "Test"))))
---
(e-call @1.2-1.13
	(e-lambda @1.3-1.8
		(args
			(p-assign @1.4-1.5 (ident "x")))
		(e-lookup-local @1.7-1.8
			(p-assign @1.4-1.5 (ident "x"))))
	(e-int @1.10-1.12 (value "42")))
---
(e-call @1.2-1.16
	(e-lambda @1.3-1.9
		(args
			(p-assign @1.4-1.5 (ident "x")))
		(e-unary-not @1.7-1.9
			(e-lookup-local @1.8-1.9
				(p-assign @1.4-1.5 (ident "x")))))
	(e-nominal @1.11-1.15 (nominal "Bool")
		(e-tag @1.11-1.15 (name "True"))))
~~~
# TYPES
~~~clojure
(expr @1.2-1.17 (type "Str"))
---
(expr @1.2-1.13 (type "Num(_size)"))
---
(expr @1.2-1.16 (type "Bool"))
~~~
