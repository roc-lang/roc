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
        (tuple_literal
          (ellipsis)
          (malformed malformed:expr_unexpected_token)
        )
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
{
	answer : 42,
	launchTheNukes! : \{  } -> (...),
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:1 to 4:1

**Parse Error**
at 1:1 to 4:2

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
