# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# TOKENS
~~~text
Int OpPlus OpPlus Int ~~~
# PARSE
~~~clojure
(binop_plus
  (num_literal_i32 1)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
1 + + 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:4 to 1:6

# CANONICALIZE
~~~clojure
(Expr.binop_plus
  (Expr.num_literal_i32 1)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_plus :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~
