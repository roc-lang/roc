# META
~~~ini
description=Fibonacci fn
type=file
~~~
# SOURCE
~~~roc
module [fib]

fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent OpLessThanOrEq Int LowerIdent KwElse LowerIdent OpenRound LowerIdent OpBinaryMinus Int CloseRound OpPlus LowerIdent OpenRound LowerIdent OpBinaryMinus Int CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_equals
    (lc "fib")
    (lambda
      (body
        (if_else <12 branches>)
      )
      (args
        (lc "n")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [fib]

fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 3:11 to 3:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_equals
    (Expr.lookup "fib")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
fib : _a
~~~
