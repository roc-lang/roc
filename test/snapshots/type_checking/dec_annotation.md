# META
~~~ini
description=Dec type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Dec
x = 123.456
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x : Dec
x = 123.456
~~~
# EXPECTED
NIL
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
    (Expr.frac_literal_big big:<idx:8>)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
