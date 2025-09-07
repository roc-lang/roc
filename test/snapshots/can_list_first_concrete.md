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
  (num_literal_i32 42)
  (str_literal_big "world")
  (frac_literal_small 3.14)
)
~~~
# FORMATTED
~~~roc
[42, "world", 3.14]
~~~
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - can_list_first_concrete.md:1:2:1:2
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
