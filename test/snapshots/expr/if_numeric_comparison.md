# META
~~~ini
description=If expression with numeric comparison
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 1 else 2
~~~
# TOKENS
~~~text
KwIf Int OpGreaterThan Int Int KwElse Int ~~~
# PARSE
~~~clojure
(if_else
  (condition     (binop_gt
      (num_literal_i32 5)
      (num_literal_i32 3)
    )
)
  (then     (num_literal_i32 1)
)
  (else     (num_literal_i32 2)
))
~~~
# FORMATTED
~~~roc
if 5 > 3 1 else 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
; Total type variables: 7
(var #0 _)
(var #1 Num *)
(var #2 Num *)
(var #3 _)
(var #4 Num *)
(var #5 Num *)
(var #6 _)
~~~
# TYPES
~~~roc
~~~
