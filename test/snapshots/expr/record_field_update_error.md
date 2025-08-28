# META
~~~ini
description=Record with field update using old syntax (should give nice error message)
type=expr
~~~
# SOURCE
~~~roc
{ person & age: 31 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpAmpersand LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(block
  (lc "person")
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "age")
    (num_literal_i32 31)
  )
)
~~~
# FORMATTED
~~~roc
person
age : 31
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:10 to 1:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.str_literal_big)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
