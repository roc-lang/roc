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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**pattern_as_basic.md:2:5:2:29:**
```roc
    (x, y) as point => point
```
    ^^^^^^^^^^^^^^^^^^^^^^^^


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
