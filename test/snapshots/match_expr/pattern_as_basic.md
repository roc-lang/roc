# META
~~~ini
description=Basic as pattern to bind both pattern and whole value
type=expr
~~~
# SOURCE
~~~roc
match (1, 2) {
    (x, y) as point => point
}
~~~
# TOKENS
~~~text
KwMatch OpenRound Int Comma Int CloseRound OpenCurly OpenRound LowerIdent Comma LowerIdent CloseRound KwAs LowerIdent OpFatArrow LowerIdent CloseCurly ~~~
# PARSE
~~~clojure
(match
  (scrutinee     (tuple_literal
      (num_literal_i32 1)
      (num_literal_i32 2)
    )
)
  (branch1     (binop_thick_arrow
      (binop_as
        (tuple_literal
          (lc "x")
          (lc "y")
        )
        (lc "point")
      )
      (lc "point")
    )
))
~~~
# FORMATTED
~~~roc
match (1, 2)
	(x, y) as point => point
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 2:21 to 2:23

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
