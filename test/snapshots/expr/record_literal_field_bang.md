# META
~~~ini
description=record_literal_field_bang
type=expr
~~~
# SOURCE
~~~roc
{
    answer: 42,
    launchTheNukes!: |{}| ...,
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon Int Comma LowerIdent OpBang OpColon OpBar OpenCurly CloseCurly OpBar TripleDot Comma CloseCurly ~~~
# PARSE
~~~clojure
(record_literal
  (binop_colon
    (lc "answer")
    (num_literal_i32 42)
  )
  (binop_colon
    (not_lc "launchTheNukes")
    (lambda
      (body
        (ellipsis)
      )
      (args
        (record_literal)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
{ answer: 42, launchTheNukes!: |{}| ... }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.record_field
    (Expr.malformed)
    (Expr.num_literal_i32 42)
  )
  (Expr.record_field
    (Expr.malformed)
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 15
(var #0 _)
(var #1 _)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #12)
(var #8 _)
(var #9 -> #14)
(var #10 _)
(var #11 _)
(var #12 fn_pure)
(var #13 {})
(var #14 record)
~~~
# TYPES
~~~roc
~~~
