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
KwApp OpenCurly LowerIdent OpColon String KwPlatform OpenSquare LowerIdent OpBang CloseSquare CloseCurly LowerIdent OpAssign Int LowerIdent OpAssign LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
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
app { pf: ("../basic-cli/platform.roc" platform [main]) }

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
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
