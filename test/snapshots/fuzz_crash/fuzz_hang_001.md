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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
