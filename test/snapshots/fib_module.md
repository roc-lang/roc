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
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpAssign OpBar LowerIdent OpBar KwIf LowerIdent OpLessThanOrEq Int LowerIdent KwElse LowerIdent OpenRound LowerIdent OpBinaryMinus Int CloseRound OpPlus LowerIdent OpenRound LowerIdent OpBinaryMinus Int CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "fib")
))
~~~
# FORMATTED
~~~roc
module [fib]

fib = |n| if n <= 1 n else fib(n - 1) + fib(n - 2)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "fib"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
