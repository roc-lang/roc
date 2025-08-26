# META
~~~ini
description=Large scientific notation that fits in Dec
type=expr
~~~
# SOURCE
~~~roc
1.5e18
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
