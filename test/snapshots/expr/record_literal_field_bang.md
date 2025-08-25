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
    (tuple_literal
      (num_literal_i32 42)
      (lc "launchTheNukes")
    )
  )
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (record_literal)
    )
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
 {
	answer: (
		42,
		launchTheNukes,
	),
	(<malformed>! | {  }) | ...,
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:20 to 3:20

**Unsupported Node**
at 3:19 to 3:20

**Unsupported Node**
at 3:19 to 3:20

**Unsupported Node**
at 3:23 to 3:24

**Unsupported Node**
at 3:27 to 3:30

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.binop_colon
    (Expr.lookup "answer")
    (Expr.malformed)
  )
  (Expr.lambda)
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
