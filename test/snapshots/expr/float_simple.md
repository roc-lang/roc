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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_not_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_not_equals :type "[True, False]_others")
~~~
# TYPES
~~~roc
[True, False]_others
~~~
