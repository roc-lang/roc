# META
~~~ini
description=Invalid float literal too many decimal points
type=expr
~~~
# SOURCE
~~~roc
3.14.15
~~~
# TOKENS
~~~text
Float Dot Int ~~~
# PARSE
~~~clojure
(binop_dot
  (frac_literal_small 3.14)
  (num_literal_i32 15)
)
~~~
# FORMATTED
~~~roc
3.14.15.15
~~~
# EXPECTED
PARSE ERROR - float_invalid.md:1:5:1:8
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 F64)
(var #2 Num *)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
