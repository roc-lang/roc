# META
~~~ini
description=Lambda with multiple arguments
type=expr
~~~
# SOURCE
~~~roc
|x, y| x + y
~~~
# TOKENS
~~~text
OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpPlus LowerIdent ~~~
# PARSE
~~~clojure
(lambda
  (body
    (binop_plus
      (lc "x")
      (lc "y")
    )
  )
  (args
    (lc "x")
    (lc "y")
  )
)
~~~
# FORMATTED
~~~roc
|x, y| x + y
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
; Total type variables: 11
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 -> #5)
(var #5 _)
(var #6 -> #10)
(var #7 _)
(var #8 _)
(var #9 fn_pure)
(var #10 fn_pure)
~~~
# TYPES
~~~roc
x : _a
y : _a
~~~
