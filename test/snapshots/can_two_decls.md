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
(block
  (binop_equals
    (lc "a")
    (num_literal_i32 5)
  )
  (binop_equals
    (lc "b")
    (binop_plus
      (lc "a")
      (num_literal_i32 1)
    )
  )
)
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
; Total type variables: 16
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 -> #8)
(var #8 Num *)
(var #9 _)
(var #10 -> #13)
(var #11 -> #12)
(var #12 -> #13)
(var #13 Num *)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
b : Num(_size)
a : Num(_size)
~~~
