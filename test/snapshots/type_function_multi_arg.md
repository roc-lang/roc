# META
~~~ini
description=Multi-argument function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

curry : (_a, _b -> _c) -> (_a -> _b -> _c)
curry = |fn| |x| |y| fn(x, y)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "curry")
    (binop_thin_arrow
      (binop_thin_arrow
        (lc "_a")
        (binop_thin_arrow
          (lc "_b")
          (lc "_c")
        )
      )
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "_a")
          (lc "_b")
        )
        (lc "_c")
      )
    )
  )
  (binop_equals
    (lc "curry")
    (lambda
      (body
        (lambda
          (body
            (lambda
              (body
                (apply_lc
                  (lc "fn")
                  (tuple_literal
                    (lc "x")
                    (lc "y")
                  )
                )
              )
              (args
                (lc "y")
              )
            )
          )
          (args
            (lc "x")
          )
        )
      )
      (args
        (lc "fn")
      )
    )
  )
  (lc "main")
  (binop_pipe
    (binop_pipe
      (unary_not <unary>)
      (underscore)
    )
    (record_literal)
  )
)
~~~
# FORMATTED
~~~roc
app { pf: ("../basic-cli/main.roc" platform [main]) }

curry: ((_a -> (_b -> _c)) -> ((_a -> _b) -> _c))
curry = \fn -> \x -> \y -> fn((x, y))

main
(<malformed>! | _) | {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:10 to 3:42

**Unsupported Node**
at 4:9 to 4:14

**Unsupported Node**
at 6:5 to 6:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "curry")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.lookup "main")
  (Expr.lambda)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_arg, _arg2 -> {}")
~~~
# TYPES
~~~roc
~~~
