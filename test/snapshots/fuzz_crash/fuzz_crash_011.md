# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module P]F
~~~
# TOKENS
~~~text
KwModule UpperIdent CloseSquare UpperIdent ~~~
# PARSE
~~~clojure
(block
  (uc "P")
  (malformed malformed:expr_unexpected_token)
  (uc "F")
)
~~~
# FORMATTED
~~~roc
module [P, , F]

P
F
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:8

**Parse Error**
at 1:9 to 1:9

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
