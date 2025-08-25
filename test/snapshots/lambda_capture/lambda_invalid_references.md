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
\x -> \y -> x + z
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:5

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
