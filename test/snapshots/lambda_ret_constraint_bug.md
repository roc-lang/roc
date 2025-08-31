# META
~~~ini
description=Lambda return type constraint bug - integer literals in lambda bodies should be constrained by function signature
type=file
~~~
# SOURCE
~~~roc
app { pf: "platform/main.roc" platform [main] }

helper : I64 -> I64
helper = |n| n * 2

main : I64, I64 -> I64
main = |_, _| helper(5)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent CloseSquare CloseCurly BlankLine LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent OpStar Int BlankLine LowerIdent OpColon UpperIdent Comma UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar LowerIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "platform/main.roc")
        (block
          (lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "platform/main.roc" platform [main] }


helper : I64 -> I64
helper = |n| n * 2
main : I64 -> I64 -> I64
main = |_, _| helper(5)
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
