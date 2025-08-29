# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(binop_plus
  (binop_pipe
    (lc "person")
    (dot_lc "age")
  )
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
person.age + 5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.binop_plus
  (Expr.lambda)
  (Expr.num_literal_i32 5)
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_plus :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
