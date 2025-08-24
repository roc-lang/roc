# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
mo|%
~~~
# TOKENS
~~~text
LowerIdent OpBar OpPercent ~~~
# PARSE
~~~clojure
(block
  (binop_pipe
    (lc "mo")
    (malformed malformed:expr_unexpected_token)
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
MISSING HEADER - fuzz_crash_001.md:1:1:1:3
PARSE ERROR - fuzz_crash_001.md:1:3:1:4
PARSE ERROR - fuzz_crash_001.md:1:4:1:5
# PROBLEMS
**Parse Error**
at 1:4 to 1:4

**Unsupported Node**
at 1:4 to 1:4

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
