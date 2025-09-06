# META
~~~ini
description=Invalid integer literal that exceeds i128 range
type=expr
~~~
# SOURCE
~~~roc
99999999999999999999999999999999999999999
~~~
# TOKENS
~~~text
Int ~~~
# PARSE
~~~clojure
(num_literal_big big:<idx:42>)
~~~
# FORMATTED
~~~roc
99999999999999999999999999999999999999999
~~~
# EXPECTED
INVALID NUMBER - expr_int_invalid.md:1:1:1:42
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.num_literal_big)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
