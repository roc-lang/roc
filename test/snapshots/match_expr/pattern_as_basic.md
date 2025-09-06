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
UNUSED VARIABLE - pattern_as_basic.md:2:6:2:7
UNUSED VARIABLE - pattern_as_basic.md:2:9:2:10
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
; Total type variables: 12
(var #0 _)
(var #1 Num *)
(var #2 Num *)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
~~~
# TYPES
~~~roc
x : _a
y : _a
point : _a
~~~
