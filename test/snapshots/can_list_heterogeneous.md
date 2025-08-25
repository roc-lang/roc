# META
~~~ini
description=Heterogeneous list causes type mismatch
type=expr
~~~
# SOURCE
~~~roc
[1, "hello", 3.14]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String Comma Float CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (tuple_literal
    (num_literal_i32 1)
    (str_literal_big "hello")
    (frac_literal_small 3.14)
  )
)
~~~
# FORMATTED
~~~roc
[(1, "hello", 3.14)]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:18

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
