# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
= "te
~~~
# TOKENS
~~~text
OpAssign MalformedString ~~~
# PARSE
~~~clojure
(block
  (malformed malformed:expr_unexpected_token)
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc

~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:1

**Parse Error**
at 1:3 to 1:3

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
