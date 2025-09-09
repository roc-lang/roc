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
; Total type variables: 28
(var #0 _)
(var #1 _)
(var #2 -> #27)
(var #3 _)
(var #4 -> #5)
(var #5 -> #6)
(var #6 -> #24)
(var #7 -> #18)
(var #8 -> #25)
(var #9 -> #10)
(var #10 -> #11)
(var #11 Num *)
(var #12 -> #17)
(var #13 -> #26)
(var #14 -> #15)
(var #15 -> #16)
(var #16 Num *)
(var #17 -> #18)
(var #18 _)
(var #19 -> #18)
(var #20 -> #27)
(var #21 _)
(var #22 _)
(var #23 _)
(var #24 Num *)
(var #25 fn_pure)
(var #26 fn_pure)
(var #27 fn_pure)
~~~
# TYPES
~~~roc
n : _a
~~~
