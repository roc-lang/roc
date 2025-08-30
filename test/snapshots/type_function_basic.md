# META
~~~ini
description=Basic function type canonicalization
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

apply : (_a -> _b) -> _a -> _b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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

apply :
	((_a -> _b) -> _a) -> _b
apply = |fn, x| fn(x)
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
    (Expr.lookup "apply")
    (Expr.binop_thin_arrow
      (Expr.binop_thin_arrow
        (Expr.binop_thin_arrow
          (Expr.lookup "_a")
          (Expr.lookup "_b")
        )
        (Expr.lookup "_a")
      )
      (Expr.lookup "_b")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "apply")
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
apply : _a
~~~
