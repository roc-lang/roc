# META
~~~ini
description=Simple subtraction using an unbound integer and an unbound frac (defaults to Dec)
type=repl
~~~
# SOURCE
~~~roc
» 1 - 0.2
~~~
# OUTPUT
0.8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(e-binop @1.1-1.1 (op "sub")
	(e-int @1.1-1.1 (value "1"))
	(e-dec-small @1.1-1.1 (numerator "2") (denominator-power-of-ten "1") (value "0.2")))
(e-binop @1.2-1.9 (op "sub")
	(e-int @1.2-1.3 (value "1"))
	(e-dec-small @1.6-1.9 (numerator "2") (denominator-power-of-ten "1") (value "0.2")))
~~~
# TYPES
~~~clojure
(expr @1.1-1.1 (type "Num(_size)"))
(expr @1.2-1.9 (type "Num(_size)"))
~~~
