# META
~~~ini
description=Dec literal with negative exponent scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.23456789012345678e-10
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_big big:<idx:0>)
~~~
# FORMATTED
~~~roc
1.23456789012345678e-10
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_big big:<idx:0>)
~~~
# SOLVED
~~~clojure
(expr :tag frac_literal_big :type "F64")
~~~
# TYPES
~~~roc
F64
~~~
