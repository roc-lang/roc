# META
~~~ini
description=Let-polymorphism error case - incompatible list elements
type=expr
~~~
# SOURCE
~~~roc
[42, 4.2, "hello"]
~~~
# TOKENS
~~~text
OpenSquare Int Comma Float Comma String CloseSquare ~~~
# PARSE
~~~clojure
(list_literal
  (tuple_literal
    (num_literal_i32 42)
    (frac_literal_small 4.2)
    (str_literal_big "hello")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - let_polymorphism_error.md:1:6:1:6
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
