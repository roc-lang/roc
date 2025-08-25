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
\(x, y) -> x + y
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:8

# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
