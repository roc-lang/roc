# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
# TOKENS
~~~text
Int OpenRound ~~~
# PARSE
~~~clojure
(block
  (apply_anon
    (num_literal_i32 0)
  )
)
~~~
# FORMATTED
~~~roc
0()
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:4

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.apply_ident)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
