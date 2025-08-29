# META
~~~ini
description=Lambda expression
type=expr
~~~
# SOURCE
~~~roc
|x| x + 1
~~~
# TOKENS
~~~text
OpBar LowerIdent OpBar LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(lambda
  (body
    (binop_plus
      (lc "x")
      (num_literal_i32 1)
    )
  )
  (args
    (lc "x")
  )
)
~~~
# FORMATTED
~~~roc
|x| x + 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lambda)
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
