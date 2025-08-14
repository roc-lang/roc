# META
~~~ini
description=Simple definitions
type=repl
~~~
# SOURCE
~~~roc
» x = 1
» y = 2
» x + y
~~~
# OUTPUT
assigned `x`
---
assigned `y`
---
3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.10
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.10 (value "1")))
	(s-let @3.5-3.10
		(p-assign @3.5-3.6 (ident "y"))
		(e-int @3.9-3.10 (value "2")))
	(e-binop @4.5-4.10 (op "add")
		(e-lookup-local @4.5-4.6
			(p-assign @2.5-2.6 (ident "x")))
		(e-lookup-local @4.9-4.10
			(p-assign @3.5-3.6 (ident "y")))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "Num(_size)"))
~~~
