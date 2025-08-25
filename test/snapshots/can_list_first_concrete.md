# META
~~~ini
description=Heterogeneous list where first element is concrete
type=expr
~~~
# SOURCE
~~~roc
[42, "world", 3.14]
~~~
# TOKENS
~~~text
OpenSquare Int Comma String Comma Float CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (tuple_literal
    (num_literal_i32 42)
    (str_literal_big "world")
    (frac_literal_small 3.14)
  )
)
~~~
# FORMATTED
~~~roc
[(42, "world", 3.14)]
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:19

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
