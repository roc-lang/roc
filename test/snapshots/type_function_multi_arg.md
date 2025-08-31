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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpColon OpenRound LowerIdent Comma LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent OpBar OpBar LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
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

curry :
	(_a -> _b -> _c) -> (_a -> _b) -> _c
curry = |fn| |x| |y| fn((x, y))

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "curry")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "curry"))
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
