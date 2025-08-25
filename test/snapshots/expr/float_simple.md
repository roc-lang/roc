# META
~~~ini
description=Simple float literal
type=expr
~~~
# SOURCE
~~~roc
3.14
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_small 3.14)
~~~
# FORMATTED
~~~roc
3.14
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_small 3.14)
~~~
# SOLVED
~~~clojure
(expr :tag frac_literal_small :type "F64")
~~~
# TYPES
~~~roc
F64
~~~
