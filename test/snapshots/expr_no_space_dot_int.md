# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = asd.0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpAssign LowerIdent Dot Int ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "foo")
    (binop_pipe
      (lc "asd")
      (num_literal_i32 0)
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

foo = asd | 0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "foo")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
foo : _a
~~~
