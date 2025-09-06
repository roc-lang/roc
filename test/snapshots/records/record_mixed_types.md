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
    (binop_dot
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
    (frac_literal_big big:<idx:20>)
  )
)
~~~
# FORMATTED
~~~roc
{ name: "Alice", age: 30, active: Bool..true, scores: [95, 87, 92], balance: 1250.75 }
~~~
# EXPECTED
UNDEFINED VARIABLE - record_mixed_types.md:1:35:1:44
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.record_field
    (Expr.malformed)
    (Expr.str_literal_big)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.num_literal_i32 30)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.record_access)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.list_literal)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.frac_literal_big big:<idx:20>)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 23
(var #0 _)
(var #1 _)
(var #2 Str)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 Num *)
(var #14 Num *)
(var #15 Num *)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 F64)
(var #20 _)
(var #21 -> #22)
(var #22 {})
~~~
# TYPES
~~~roc
~~~
