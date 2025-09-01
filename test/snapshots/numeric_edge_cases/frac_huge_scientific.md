# META
~~~ini
description=Very large number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e100
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_big big:<idx:8>)
~~~
# FORMATTED
~~~roc
1.0e100
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_big big:<idx:8>)
~~~
# SOLVED
~~~clojure
(expr :tag frac_literal_big :type "F64")
~~~
# TYPES
~~~roc
F64
~~~
