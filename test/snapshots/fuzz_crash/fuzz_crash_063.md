# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}0}.a
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly Int CloseCurly Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (binop_pipe
    (malformed malformed:expr_unexpected_token)
    (dot_lc "a")
  )
)
~~~
# FORMATTED
~~~roc
module [}]

0} | .a
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:8

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 1:10 to 1:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_star)
  (Expr.frac_literal_big)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
