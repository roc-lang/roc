# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[){..0,)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseRound OpenCurly DoubleDot Int Comma CloseRound ~~~
# PARSE
~~~clojure
(block
  (record_literal
    (underscore)
  )
  (num_literal_i32 0)
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - fuzz_crash_053.md:1:8:1:9
PARSE ERROR - fuzz_crash_053.md:1:15:1:15
# PROBLEMS
**Parse Error**
at 1:8 to 1:8

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 1:9 to 1:12

**Parse Error**
at 1:13 to 1:13

**Parse Error**
at 1:14 to 1:14

**Pattern in Expression Context**
at 1:10 to 1:11

**Unsupported Node**
at 1:13 to 1:13

**Unsupported Node**
at 1:14 to 1:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.record_literal
    (Expr.malformed)
  )
  (Expr.num_literal_i32 0)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
