# META
~~~ini
description=Error handling for invalid variable references in lambda captures
type=expr
~~~
# SOURCE
~~~roc
|x| |y| x + z
~~~
# TOKENS
~~~text
OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpPlus LowerIdent ~~~
# PARSE
~~~clojure
(lambda
  (body
    (lambda
      (body
        (binop_plus
          (lc "x")
          (lc "z")
        )
      )
      (args
        (lc "y")
      )
    )
  )
  (args
    (lc "x")
  )
)
~~~
# FORMATTED
~~~roc
|x| |y| x + z
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
