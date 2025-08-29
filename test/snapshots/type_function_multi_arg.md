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
  (binop_equals
    (not_lc "main")
    (lambda
      (body
        (record_literal)
      )
      (args
        (underscore)
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main] }

curry :
	(_a -> _b -> _c) -> (_a -> _b) -> _c
curry = |fn| |x| |y| fn((x, y))
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "curry")
    (Expr.binop_thin_arrow)
  )
  (Expr.binop_equals
    (Expr.lookup "curry")
    (Expr.lambda)
  )
  (Expr.binop_equals
    (Expr.not_lookup)
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
curry : _a
~~~
