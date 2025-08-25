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
(block
  (binop_colon
    (lc "name")
    (binop_colon
      (tuple_literal
        (binop_colon
          (tuple_literal
            (binop_colon
              (tuple_literal
                (binop_colon
                  (tuple_literal
                    (str_literal_big "Alice")
                    (lc "age")
                  )
                  (num_literal_i32 30)
                )
                (lc "active")
              )
              (binop_pipe
                (uc "Bool")
                (dot_lc "true")
              )
            )
            (lc "scores")
          )
          (list_literal
            (tuple_literal
              (num_literal_i32 95)
              (num_literal_i32 87)
              (num_literal_i32 92)
            )
          )
        )
        (lc "balance")
      )
      (frac_literal_big big:<idx:6>)
    )
  )
)
~~~
# FORMATTED
~~~roc
name: ((((("Alice", age): 30, active): Bool | .true, scores): [(95, 87, 92)], balance): 1250.75)
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:75 to 1:76

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "name")
    (Expr.binop_colon
      (Expr.malformed)
      (Expr.frac_literal_big big:<idx:6>)
    )
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
