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
(module-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
module [)]

{ _ }0,)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:9

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 1:9 to 1:12

**Parse Error**
at 1:13 to 1:14

**Parse Error**
at 1:14 to 1:15

**Pattern in Expression Context**
at 1:10 to 1:10

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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
