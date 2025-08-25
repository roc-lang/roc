# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# TOKENS
~~~text
OpBar OpBar Int ~~~
# PARSE
~~~clojure
(block
  (lambda
    (body
      (num_literal_i32 1)
    )
  )
)
~~~
# FORMATTED
~~~roc
\ -> 1
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
