# META
~~~ini
description=Record with mixed field types
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", age: 30, active: Bool.true, scores: [95, 87, 92], balance: 1250.75 }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma LowerIdent OpColon Int Comma LowerIdent OpColon UpperIdent Dot LowerIdent Comma LowerIdent OpColon OpenSquare Int Comma Int Comma Int CloseSquare Comma LowerIdent OpColon Float CloseCurly ~~~
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
    (lc "active")
    (binop_pipe
      (uc "Bool")
      (dot_lc "true")
    )
  )
  (binop_colon
    (lc "scores")
    (list_literal
      (num_literal_i32 95)
      (num_literal_i32 87)
      (num_literal_i32 92)
    )
  )
  (binop_colon
    (lc "balance")
    (frac_literal_big big:<idx:6>)
  )
)
~~~
# FORMATTED
~~~roc
{ name : "Alice", age : 30, active : Bool.true, scores : [95, 87, 92], balance : 1250.75 }
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
