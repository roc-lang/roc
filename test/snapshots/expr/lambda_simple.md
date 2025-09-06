# META
~~~ini
description=Lambda expression
type=expr
~~~
# SOURCE
~~~roc
|x| x + 1
~~~
# TOKENS
~~~text
OpBar LowerIdent OpBar LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(lambda
  (body
    (binop_plus
      (lc "x")
      (num_literal_i32 1)
    )
  )
  (args
    (lc "x")
  )
)
~~~
# FORMATTED
~~~roc
|x| x + 1
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
(var #2 -> #3)
(var #3 -> #4)
(var #4 Num *)
(var #5 -> #7)
(var #6 _)
(var #7 fn_pure)
~~~
# TYPES
~~~roc
x : _a
~~~
