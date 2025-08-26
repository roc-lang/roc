# META
~~~ini
description=Record with duplicate field names (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, name: "Bob", email: "alice@example.com", age: 25 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon String Comma LowerIdent OpColon String Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "name")
    (str_literal_big "Alice")
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 30)
  )
  (binop_colon
    (lc "name")
    (str_literal_small "Bob")
  )
  (binop_colon
    (lc "email")
    (str_literal_big "alice@example.com")
  )
  (binop_colon
    (lc "age")
    (num_literal_i32 25)
  )
)
~~~
# FORMATTED
~~~roc
{ name : "Alice", age : 30, name : "Bob", email : "alice@example.com", age : 25 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_double_slash)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_slash :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
