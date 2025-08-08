# META
~~~ini
description=Debug boolean with trace
type=repl
~~~
# SOURCE
~~~roc
» (|x| !x)(True)
~~~
# OUTPUT
False
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
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
(expr @1.2-1.16 (type "Bool"))
~~~
