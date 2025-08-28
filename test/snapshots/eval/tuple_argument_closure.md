# META
~~~ini
description=Tuple as an argument
type=expr
~~~
# SOURCE
~~~roc
(|(x,y)| x * y )((1,2))
~~~
# TOKENS
~~~text
OpenRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar LowerIdent OpStar LowerIdent CloseRound OpenRound OpenRound Int Comma Int CloseRound CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (lambda
    (body
      (binop_star
        (lc "x")
        (lc "y")
      )
    )
    (args
      (tuple_literal
        (lc "x")
        (lc "y")
      )
    )
  )
  (tuple_literal
    (num_literal_i32 1)
    (num_literal_i32 2)
  )
)
~~~
# FORMATTED
~~~roc
|x, y| x * y((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_equals)
~~~
# SOLVED
~~~clojure
(expr :tag binop_equals :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
