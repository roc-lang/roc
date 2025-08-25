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
(record_literal
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
    (num_literal_i32 5)
  )
  (binop_colon
    (lc "y")
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
{
	Foo.Bar | .baz,
	x: 5,
	y: 0,
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:15 to 1:15

**Unsupported Node**
at 1:3 to 1:6

**Unsupported Node**
at 1:6 to 1:9

**Unsupported Node**
at 1:15 to 1:15

# CANONICALIZE
~~~clojure
(Expr.record_literal
  (Expr.lambda)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "x")
    (Expr.num_literal_i32 5)
  )
  (Expr.binop_colon
    (Expr.lookup "y")
    (Expr.num_literal_i32 0)
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
