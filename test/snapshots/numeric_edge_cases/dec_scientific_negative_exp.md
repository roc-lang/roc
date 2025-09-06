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
(frac_literal_big big:<idx:24>)
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
(Expr.frac_literal_big big:<idx:24>)
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
