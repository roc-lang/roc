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
  (str_literal_big "hello")
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
; Total type variables: 6
(var #0 _)
(var #1 <error>)
(var #2 -> #1)
(var #3 -> #1)
(var #4 -> #5)
(var #5 List #1)
~~~
# TYPES
~~~roc
~~~
