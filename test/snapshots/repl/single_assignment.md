# META
~~~ini
description=Simple assignment and variable access
type=repl
~~~
# SOURCE
~~~roc
» x = 5
» x
~~~
# OUTPUT
assigned `x`
---
5
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-block @1.1-4.2
	(s-let @2.5-2.10
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.10 (value "5")))
	(e-lookup-local @3.5-3.6
		(p-assign @2.5-2.6 (ident "x"))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "Num(_size)"))
~~~
