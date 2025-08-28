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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_lte)
~~~
# SOLVED
~~~clojure
(expr :tag binop_lte :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
