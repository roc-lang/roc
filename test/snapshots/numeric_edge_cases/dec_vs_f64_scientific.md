# META
~~~ini
description=Scientific notation uses Dec when value fits
type=expr
~~~
# SOURCE
~~~roc
1.5e2
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_big big:<idx:6>)
~~~
# FORMATTED
~~~roc
1.5e2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_big big:<idx:6>)
~~~
# SOLVED
~~~clojure
(expr :tag frac_literal_big :type "F64")
~~~
# TYPES
~~~roc
F64
~~~
