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
(e-int @1.1-1.1 (value "42"))
(e-int @1.2-1.4 (value "42"))
---
(e-dec-small @1.1-1.1 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
(e-dec-small @1.2-1.6 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
---
(e-binop @1.1-1.1 (op "add")
	(e-int @1.1-1.1 (value "1"))
	(e-int @1.1-1.1 (value "2")))
(e-binop @1.2-1.7 (op "add")
	(e-int @1.2-1.3 (value "1"))
	(e-int @1.6-1.7 (value "2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.4 (type "Num(_size)"))
---
(expr @1.1-1.1 (type "Frac(_size)"))
(expr @1.2-1.6 (type "Frac(_size)"))
---
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.7 (type "Num(_size)"))
~~~
