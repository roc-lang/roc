# META
~~~ini
description=Decimal just above dec_small limit that requires frac_dec
type=expr
~~~
# SOURCE
~~~roc
32768.0
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
(Expr.binop_lt)
~~~
# SOLVED
~~~clojure
(expr :tag binop_lt :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
