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
  (num_literal_i32 42)
  (frac_literal_small 4.2)
  (str_literal_big "<idx:12>")
)
~~~
# FORMATTED
~~~roc
[42, 4.2, "hello"]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - let_polymorphism_error.md:1:6:1:6
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.list_literal)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
