# META
~~~ini
description=Binary operation expression simple addition
type=expr
~~~
# SOURCE
~~~roc
1 + 2
~~~
# TOKENS
~~~text
Int OpPlus Int ~~~
# PARSE
~~~clojure
(binop_plus
  (num_literal_i32 1)
  (num_literal_i32 2)
)
~~~
# FORMATTED
~~~roc
1 + 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_plus
  (Expr.num_literal_i32 1)
  (Expr.num_literal_i32 2)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 -> #2)
(var #2 -> #3)
(var #3 Num *)
~~~
# TYPES
~~~roc
~~~
