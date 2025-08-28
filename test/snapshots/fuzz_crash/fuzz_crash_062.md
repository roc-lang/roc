# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}|0
as s|||0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpBar Int KwAs LowerIdent OpOr OpBar Int ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
module []

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:8

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 2:9 to 2:9

**Parse Error**
at 2:9 to 2:9

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
