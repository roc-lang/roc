# META
~~~ini
description=Effectful function type with fat arrow syntax
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

runEffect! : (_a => _b) -> _a => _b
runEffect! = |fn!, x| fn!(x)

main! = |_| {}
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpBang OpColon OpenRound LowerIdent OpFatArrow LowerIdent CloseRound OpArrow LowerIdent OpFatArrow LowerIdent LowerIdent OpBang OpAssign OpBar LowerIdent OpBang Comma LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent CloseRound LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (not_lc "runEffect")
    (binop_thick_arrow
      (binop_thin_arrow
        (binop_thick_arrow
          (lc "_a")
          (lc "_b")
        )
        (lc "_a")
      )
      (lc "_b")
    )
  )
  (binop_equals
    (not_lc "runEffect")
    (lambda
      (body
        (apply_anon
          (not_lc "fn")
          (lc "x")
        )
      )
      (args
        (tuple_literal
          (not_lc "fn")
          (lc "x")
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

runEffect! : (_a => _b) -> _a => _b
runEffect! = |fn!, x| fn!(x)
main! = |_| {  }
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:25 to 3:27

**Unsupported Node**
at 4:15 to 4:18

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.not_lookup)
    (Expr.binop_thick_arrow
      (Expr.malformed)
      (Expr.lookup "_b")
    )
  )
  (Expr.binop_equals
    (Expr.not_lookup)
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
~~~
