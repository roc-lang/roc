# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0|
~~~
# TOKENS
~~~text
Int OpBar ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (num_literal_i32 0)
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_016.md:1:1:1:2
PARSE ERROR - fuzz_crash_016.md:1:2:1:3
# PROBLEMS
**Parse Error**
at 1:3 to 1:3

**Unsupported Node**
at 1:3 to 1:3

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
