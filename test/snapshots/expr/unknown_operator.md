# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# TOKENS
~~~text
Int OpPlus OpPlus Int ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
2
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:4 to 1:6

**Parse Error**
at 1:6 to 1:7

**Unsupported Node**
at 1:6 to 1:7

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
