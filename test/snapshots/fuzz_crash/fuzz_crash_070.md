# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]()0     .t
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare OpenRound CloseRound Int Dot LowerIdent ~~~
# PARSE
~~~clojure
(block
  (tuple_literal)
  (binop_pipe
    (num_literal_i32 0)
    (dot_lc "t")
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_070.md:1:9:1:10
PARSE ERROR - fuzz_crash_070.md:1:10:1:11
PARSE ERROR - fuzz_crash_070.md:1:11:1:12
PARSE ERROR - fuzz_crash_070.md:1:17:1:19
# PROBLEMS
**Unsupported Node**
at 1:9 to 1:10

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg -> _ret")
~~~
# TYPES
~~~roc
~~~
