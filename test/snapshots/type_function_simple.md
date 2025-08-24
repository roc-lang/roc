# META
~~~ini
description=Simple function type in function annotation
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

apply : (_a -> _b) -> _a -> _b
apply = |fn, x| fn(x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar LowerIdent OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "apply")
    (binop_thin_arrow
      (binop_thin_arrow
        (binop_thin_arrow
          (lc "_a")
          (lc "_b")
        )
        (lc "_a")
      )
      (lc "_b")
    )
  )
  (binop_equals
    (lc "apply")
    (lambda
      (body
        (apply_lc
          (lc "fn")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (lc "fn")
          (lc "x")
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
PARSE ERROR - type_function_simple.md:3:26:3:28
PARSE ERROR - type_function_simple.md:3:29:3:31
# PROBLEMS
**Parse Error**
at 6:7 to 6:7

**Unsupported Node**
at 3:10 to 3:31

**Unsupported Node**
at 4:9 to 4:17

**Unsupported Node**
at 6:5 to 6:7

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "apply")
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
