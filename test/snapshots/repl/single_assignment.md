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
(e-block @1.1-1.1
	(s-let @1.1-1.1
		(p-assign @1.1-1.1 (ident "x"))
		(e-int @1.1-1.1 (value "5")))
	(e-lookup-local @1.1-1.1
		(p-assign @1.1-1.1 (ident "x"))))
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.3 (type "Error"))
~~~
