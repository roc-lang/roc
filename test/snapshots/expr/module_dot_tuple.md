# META
~~~ini
description=Module dot malformed (should error)
type=expr
~~~
# SOURCE
~~~roc
I.5
~~~
# TOKENS
~~~text
UpperIdent Dot Int ~~~
# PARSE
~~~clojure
(binop_pipe
  (uc "I")
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
I | 5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lambda (canonicalized))
~~~
# SOLVED
~~~clojure
(expr :tag lambda :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
