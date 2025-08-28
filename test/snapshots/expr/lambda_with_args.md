# META
~~~ini
description=Lambda with multiple arguments
type=expr
~~~
# SOURCE
~~~roc
|x, y| x + y
~~~
# TOKENS
~~~text
OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent ~~~
# PARSE
~~~clojure
(lambda
  (body
    (binop_plus
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
