# META
~~~ini
description=Unbound number types (Num, Int, Frac) should not display type annotations in REPL
type=repl
~~~
# SOURCE
~~~roc
» 42
» 3.14
» 1 + 2
~~~
# OUTPUT
42
---
3.14e0
---
3
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-int @1.1-1.3 (value "42"))
---
(e-dec-small @1.1-1.5 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
---
(e-binop @1.1-1.6 (op "add")
	(e-int @1.1-1.2 (value "1"))
	(e-int @1.5-1.6 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "Num(_size)"))
---
(expr @1.1-1.5 (type "Frac(_size)"))
---
(expr @1.1-1.6 (type "Num(_size)"))
~~~
