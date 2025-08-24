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
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - record_field_update_error.md:1:10:1:11
UNEXPECTED TOKEN IN TYPE ANNOTATION - record_field_update_error.md:1:17:1:19
UNDEFINED VARIABLE - record_field_update_error.md:1:3:1:9
MALFORMED TYPE - record_field_update_error.md:1:17:1:19
# PROBLEMS
**Parse Error**
at 1:10 to 1:10

**Unsupported Node**
at 1:10 to 1:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lookup "person")
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 31)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
