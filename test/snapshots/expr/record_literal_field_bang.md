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
	answer: 42,
	launchTheNukes!: \
		{  },
	 -> (...),
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 4:1 to 4:1

**Parse Error**
at 1:1 to 4:2

**Unsupported Node**
at 3:22 to 3:25

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "answer")
    (Expr.num_literal_i32 42)
  )
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.malformed)
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
