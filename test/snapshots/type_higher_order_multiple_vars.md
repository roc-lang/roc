# META
~~~ini
description=Higher-order function with multiple type variables
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "compose")
    (binop_thin_arrow
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "_b")
          (lc "_c")
        )
        (binop_thin_arrow
          (lc "_a")
          (lc "_b")
        )
      )
      (binop_thin_arrow
        (lc "_a")
        (lc "_c")
      )
    )
  )
  (binop_equals
    (lc "compose")
    (lambda
      (body
        (lambda
          (body
            (apply_lc
              (lc "f")
              (apply_lc
                (lc "g")
                (lc "x")
              )
            )
          )
          (args
            (lc "x")
          )
        )
      )
      (args
        (tuple_literal
          (lc "f")
          (lc "g")
        )
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

compose :
	((_b -> _c) -> _a -> _b) -> _a -> _c
compose = |f, g| |x| f(g(x))
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 4:12 to 4:16

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "compose")
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.binop_thin_arrow
          (Expr.lookup "_b")
          (Expr.lookup "_c")
        )
        (Expr.binop_thin_arrow
          (Expr.lookup "_a")
          (Expr.lookup "_b")
        )
      )
      (Expr.binop_thin_arrow
        (Expr.lookup "_a")
        (Expr.lookup "_c")
      )
    )
  )
  (Expr.binop_equals
    (Expr.lookup "compose")
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
compose : _a
~~~
