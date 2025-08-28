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
import B
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

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
