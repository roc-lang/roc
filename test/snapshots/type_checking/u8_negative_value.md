# META
~~~ini
description=U8 type annotation with negative value
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = -1
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign OpUnaryMinus Int ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x : U8
x = -1
~~~
# EXPECTED
NEGATIVE UNSIGNED INTEGER - u8_negative_value.md:4:5:4:7
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "x")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.unary_neg)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
