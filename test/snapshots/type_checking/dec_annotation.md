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
(block
  (binop_colon
    (lc "x")
    (uc "Dec")
  )
  (binop_equals
    (lc "x")
    (frac_literal_big big:<idx:8>)
  )
)
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "x"))
    (type type_2)
  )
  (Stmt.assign
    (pattern (Patt.ident "x"))
    (Expr.frac_literal_big big:<idx:8>)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 F64)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
x : F64
~~~
