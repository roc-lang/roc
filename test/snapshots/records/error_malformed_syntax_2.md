# META
~~~ini
description=Malformed record syntax using equals instead of colon (error case)
type=expr
~~~
# SOURCE
~~~roc
{ age: 42, name = "Alice" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int Comma LowerIdent OpAssign String CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "age")
    (num_literal_i32 42)
  )
  (binop_equals
    (lc "name")
    (str_literal_big "Alice")
  )
)
~~~
# FORMATTED
~~~roc
{ age : 42, name = "Alice" }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "age")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_equals
    (Expr.lookup "name")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag record_literal :type "{}")
~~~
# TYPES
~~~roc
{}
~~~
