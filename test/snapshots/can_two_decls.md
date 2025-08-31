# META
~~~ini
description=Two decls
type=file
~~~
# SOURCE
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }

a = 5
b = a + 1
~~~
# TOKENS
~~~text
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly BlankLine LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(app-header
  (packages
    (binop_colon
      (lc "pf")
      (binop_platform
        (str_literal_big "../basic-cli/platform.roc")
        (block
          (not_lc "main")
        )
      )
    )
))
~~~
# FORMATTED
~~~roc
app { pf: "../basic-cli/platform.roc" platform [main!] }


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
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
~~~
