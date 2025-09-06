# META
~~~ini
description=All fractional type annotations
type=file
~~~
# SOURCE
~~~roc
module []

a : F32
a = 3.14

b : F64
b = 2.71828

c : Dec
c = 123.456
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(module-header)
~~~
# FORMATTED
~~~roc
module []

a : F32
a = 3.14

b : F64
b = 2.71828

c : Dec
c = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Stmt.type_anno
    (name "a")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.frac_literal_small 3.14)
  )
  (Stmt.type_anno
    (name "b")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.frac_literal_big big:<idx:16>)
  )
  (Stmt.type_anno
    (name "c")
    (type uc)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.frac_literal_big big:<idx:24>)
  )
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
~~~
