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
))
~~~
# FORMATTED
~~~roc
match (1, 2)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 2:12 to 2:12

**Parse Error**
at 2:21 to 2:21

# CANONICALIZE
~~~clojure
(Expr.dot_num)
~~~
# SOLVED
~~~clojure
(expr :tag dot_num :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
