# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign Float LowerIdent OpAssign Float LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.frac_literal_small 3.14)
  )
  (Stmt.assign
    (pattern (Patt.ident "y"))
    (Expr.frac_literal_big big:<idx:8>)
  )
  (Stmt.assign
    (pattern (Patt.ident "z"))
    (Expr.frac_literal_small 0.5)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "F64")
~~~
# TYPES
~~~roc
~~~
