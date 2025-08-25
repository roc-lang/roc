# META
~~~ini
description=record_builder
type=expr
~~~
# SOURCE
~~~roc
{ Foo.Bar.baz <-
    x: 5,
    y: 0,
}
~~~
# TOKENS
~~~text
OpenCurly UpperIdent Dot UpperIdent Dot LowerIdent OpBackArrow LowerIdent OpColon Int Comma LowerIdent OpColon Int Comma CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (binop_pipe
      (uc "Foo")
      (uc "Bar")
    )
    (dot_lc "baz")
  )
  (malformed malformed:expr_unexpected_token)
  (binop_colon
    (lc "x")
    (tuple_literal
      (binop_colon
        (tuple_literal
          (num_literal_i32 5)
          (lc "y")
        )
        (num_literal_i32 0)
      )
      (malformed malformed:expr_unexpected_token)
    )
  )
)
~~~
# FORMATTED
~~~roc
(Foo.Bar) | .baz<malformed>
x: ((
	5,
	y,
): 0, <malformed>)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:15 to 1:15

**Parse Error**
at 4:1 to 4:1

**Parse Error**
at 1:1 to 4:2

**Unsupported Node**
at 1:3 to 1:6

**Unsupported Node**
at 1:6 to 1:9

**Unsupported Node**
at 1:15 to 1:15

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lambda)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "x")
    (Expr.malformed)
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
