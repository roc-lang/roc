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
    (tuple_literal
      (underscore)
      (underscore)
    )
  )
)
~~~
# FORMATTED
~~~roc
\(_, _) -> 42
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
_a
~~~
