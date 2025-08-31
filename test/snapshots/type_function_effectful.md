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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpBang OpColon OpenRound LowerIdent OpFatArrow LowerIdent CloseRound OpArrow LowerIdent OpFatArrow LowerIdent LowerIdent OpBang OpAssign OpBar LowerIdent OpBang Comma LowerIdent OpBar LowerIdent OpBang OpenRound LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }


runEffect! : (_a => _b) -> _a => _b
runEffect! = |fn!, x| fn!(x)
main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
