# META
~~~ini
description=Parameterized type alias with type variables
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/main.roc" platform [main!] }

Pair(a, b) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |(x, y)| (y, x)

main! = |_| swapPair(1, 2)
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpColon UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar OpenRound LowerIdent Comma LowerIdent CloseRound OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar LowerIdent OpenRound Int Comma Int CloseRound ~~~
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

Pair((a, b)) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |x, y| (y, x)

main! = |_| swapPair((1, 2))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name node:apply_uc)
    (type tuple_literal)
  )
  (Stmt.type_anno
    (name "swapPair")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "swapPair"))
    (Expr.lambda (canonicalized))
  )
  (Stmt.assign
    (pattern (Patt.ident "main"))
    (Expr.lambda (canonicalized))
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
