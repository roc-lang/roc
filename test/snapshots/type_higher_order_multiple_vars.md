# META
~~~ini
description=Higher-order function with multiple type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

compose : (_b -> _c) -> (_a -> _b) -> (_a -> _c)
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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
NO CHANGE
~~~
# EXPECTED
PARSE ERROR - type_higher_order_multiple_vars.md:3:36:3:38
PARSE ERROR - type_higher_order_multiple_vars.md:3:39:3:40
PARSE ERROR - type_higher_order_multiple_vars.md:3:40:3:42
PARSE ERROR - type_higher_order_multiple_vars.md:3:43:3:45
PARSE ERROR - type_higher_order_multiple_vars.md:3:46:3:48
PARSE ERROR - type_higher_order_multiple_vars.md:3:48:3:49
# PROBLEMS
**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:12 to 3:48

**Unsupported Node**
at 4:11 to 4:18

**Unsupported Node**
at 6:5 to 6:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "compose")
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
