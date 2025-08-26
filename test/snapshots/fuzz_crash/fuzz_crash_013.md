# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0{
~~~
# TOKENS
~~~text
Int OpenCurly ~~~
# PARSE
~~~clojure
(block
  (num_literal_i32 0)
  (block)
)
~~~
# FORMATTED
~~~roc
0{
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:2 to 1:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_star)
  (Expr.block
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
