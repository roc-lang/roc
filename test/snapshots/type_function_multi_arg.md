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
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
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
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.lookup "_a")
        (Expr.binop_thin_arrow
          (Expr.lookup "_b")
          (Expr.lookup "_c")
        )
      )
      (Expr.binop_thin_arrow
        (Expr.binop_thin_arrow
          (Expr.lookup "_a")
          (Expr.lookup "_b")
        )
        (Expr.lookup "_c")
      )
    )
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
