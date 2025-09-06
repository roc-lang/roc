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
(block
  (binop_colon
    (lc "a")
    (uc "F32")
  )
  (binop_equals
    (lc "a")
    (frac_literal_small 3.14)
  )
  (binop_colon
    (lc "b")
    (uc "F64")
  )
  (binop_equals
    (lc "b")
    (frac_literal_big big:<idx:16>)
  )
  (binop_colon
    (lc "c")
    (uc "Dec")
  )
  (binop_equals
    (lc "c")
    (frac_literal_big big:<idx:24>)
  )
)
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
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "a"))
    (type type_2)
  )
  (Stmt.assign
    (pattern (Patt.ident "a"))
    (Expr.frac_literal_small 3.14)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "b"))
    (type type_8)
  )
  (Stmt.assign
    (pattern (Patt.ident "b"))
    (Expr.frac_literal_big big:<idx:16>)
  )
  (Stmt.standalone_type_anno
    (pattern (Patt.ident "c"))
    (type type_14)
  )
  (Stmt.assign
    (pattern (Patt.ident "c"))
    (Expr.frac_literal_big big:<idx:24>)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 20
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 -> #5)
(var #5 F64)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 -> #11)
(var #11 F64)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
(var #16 -> #17)
(var #17 F64)
(var #18 _)
(var #19 _)
~~~
# TYPES
~~~roc
a : F64
c : F64
b : F64
~~~
