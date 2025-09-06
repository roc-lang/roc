# META
~~~ini
description=Simple Str type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Str
x = "hello"
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_colon
    (lc "x")
    (uc "Str")
  )
  (binop_equals
    (lc "x")
    (str_literal_big "hello")
  )
)
~~~
# FORMATTED
~~~roc
module []

x : Str
x = "hello"
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
    (Expr.str_literal_big)
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
(var #5 Str)
(var #6 _)
(var #7 _)
~~~
# TYPES
~~~roc
x : Str
~~~
