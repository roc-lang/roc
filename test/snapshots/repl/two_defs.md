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
(e-block @1.1-1.1
	(s-let @1.1-1.1
		(p-assign @1.1-1.1 (ident "x"))
		(e-int @1.1-1.1 (value "1")))
	(s-let @1.1-1.1
		(p-assign @1.1-1.1 (ident "y"))
		(e-int @1.1-1.1 (value "2")))
	(e-binop @1.1-1.1 (op "add")
		(e-lookup-local @1.1-1.1
			(p-assign @1.1-1.1 (ident "x")))
		(e-lookup-local @1.1-1.1
			(p-assign @1.1-1.1 (ident "y")))))
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.3 (type "Error"))
~~~
