# META
~~~ini
description=two_arg_closure
type=expr
~~~
# SOURCE
~~~roc
|_, _| 42
~~~
# TOKENS
~~~text
OpBar Underscore Comma Underscore OpBar Int ~~~
# PARSE
~~~clojure
(lambda
  (body
    (num_literal_i32 42)
  )
  (args
    (underscore)
    (underscore)
  )
)
~~~
# FORMATTED
~~~roc
|_, _| 42
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
