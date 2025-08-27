# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
*import B as
~~~
# TOKENS
~~~text
OpStar KwImport UpperIdent KwAs ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (import
    (uc "B")
  )
)
~~~
# FORMATTED
~~~roc
import
import B
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_plus)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
