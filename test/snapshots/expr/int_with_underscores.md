# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
# TOKENS
~~~text
Int Underscore Int Underscore Int ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
_
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:2 to 1:3

**Unsupported Node**
at 1:2 to 1:3

# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
