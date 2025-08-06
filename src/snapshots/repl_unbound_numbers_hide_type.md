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
(e-int @1.2-1.4 (value "42"))
---
(e-dec-small @1.2-1.6 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
---
(e-binop @1.2-1.7 (op "add")
	(e-int @1.2-1.3 (value "1"))
	(e-int @1.6-1.7 (value "2")))
~~~
