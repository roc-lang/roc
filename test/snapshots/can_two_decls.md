# META
~~~ini
description=Two decls
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

a = 5
b = a + 1
~~~
# TOKENS
~~~text
KwApp OpenSquare LowerIdent OpBang CloseSquare OpenCurly LowerIdent OpColon KwPlatform String CloseCurly BlankLine LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int ~~~
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
        (str_literal_big "../basic-cli/platform.roc")
        (block)
      )
    )
))
~~~
# FORMATTED
~~~roc
app [main!] { pf: "../basic-cli/platform.roc" platform [] }

a = 5
b = a + 1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.num_literal_i32 5)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.binop_plus
      (Expr.lookup "a")
      (Expr.num_literal_i32 1)
    )
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
