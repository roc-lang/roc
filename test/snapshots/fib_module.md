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
(block
  (binop_equals
    (lc "fib")
    (lambda
      (body
        (if_else
          (condition             (binop_lte
              (lc "n")
              (num_literal_i32 1)
            )
)
          (then             (lc "n")
)
          (else             (binop_plus
              (apply_lc
                (lc "fib")
                (binop_minus
                  (lc "n")
                  (num_literal_i32 1)
                )
              )
              (apply_lc
                (lc "fib")
                (binop_minus
                  (lc "n")
                  (num_literal_i32 2)
                )
              )
            )
))
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
; Total type variables: 25
(var #0 _)
(var #1 _)
(var #2 -> #24)
(var #3 _)
(var #4 _)
(var #5 Num *)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 Num *)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 Num *)
(var #16 _)
(var #17 _)
(var #18 _)
(var #19 _)
(var #20 -> #24)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 fn_pure)
~~~
# TYPES
~~~roc
n : _a
~~~
