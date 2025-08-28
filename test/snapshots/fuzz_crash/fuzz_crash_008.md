# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# TOKENS
~~~text
OpBar OpBar Int ~~~
# PARSE
~~~clojure
(block
  (lambda
    (body
      (num_literal_i32 1)
    )
  )
)
~~~
# FORMATTED
~~~roc
|| 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
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
