# META
~~~ini
description=Very small number in scientific notation
type=expr
~~~
# SOURCE
~~~roc
1.0e-100
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_big frac:<idx:9>)
~~~
# FORMATTED
~~~roc
1.0e-100
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_big big:<idx:9>)
~~~
# SOLVED
~~~clojure
; Total type variables: 2
(var #0 _)
(var #1 F64)
~~~
# TYPES
~~~roc
~~~
