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
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpColon OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound OpArrow OpenRound LowerIdent OpArrow LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpBar LowerIdent OpBar LowerIdent OpenRound LowerIdent OpenRound LowerIdent CloseRound CloseRound BlankLine LowerIdent OpBang OpAssign OpBar Underscore OpBar OpenCurly CloseCurly ~~~
# PARSE
~~~clojure
(app-header
  (exposes
    (not_lc "main")
)
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/main.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/main.roc" platform [] }

compose :
	((_b -> _c) -> _a -> _b) -> _a -> _c
compose = |f, g| |x| f(g(x))

main! = |_| {}
~~~
# EXPECTED
PARSE ERROR - type_higher_order_multiple_vars.md:3:36:3:38
PARSE ERROR - type_higher_order_multiple_vars.md:3:39:3:40
PARSE ERROR - type_higher_order_multiple_vars.md:3:40:3:42
PARSE ERROR - type_higher_order_multiple_vars.md:3:43:3:45
PARSE ERROR - type_higher_order_multiple_vars.md:3:46:3:48
PARSE ERROR - type_higher_order_multiple_vars.md:3:48:3:49
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "compose")
    (type binop_thin_arrow)
  )
  (Stmt.assign
    (pattern (Patt.ident "compose"))
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
~~~
# TYPES
~~~roc
~~~
