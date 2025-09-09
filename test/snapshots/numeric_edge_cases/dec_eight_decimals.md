# META
~~~ini
description=Decimal with 8 decimal places
type=expr
~~~
# SOURCE
~~~roc
3.14159265
~~~
# TOKENS
~~~text
Float ~~~
# PARSE
~~~clojure
(frac_literal_big frac:<idx:11>)
~~~
# FORMATTED
~~~roc
3.14159265
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.frac_literal_big big:<idx:11>)
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
