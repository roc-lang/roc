# META
~~~ini
description=Simple string closure
type=repl
~~~
# SOURCE
~~~roc
» (|s| s)("Test")
~~~
# OUTPUT
"Test"
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
~~~
# TYPES
~~~clojure
(expr @1.2-1.17 (type "Str"))
~~~
