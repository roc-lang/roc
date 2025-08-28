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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_accessor)
~~~
# SOLVED
~~~clojure
(expr :tag record_accessor :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
