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
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:1 to 1:8

# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
