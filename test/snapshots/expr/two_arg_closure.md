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
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 Num *)
(var #4 -> #7)
(var #5 _)
(var #6 _)
(var #7 fn_pure)
~~~
# TYPES
~~~roc
~~~
