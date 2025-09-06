# META
~~~ini
description=Hexadecimal integer literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 0xFF
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare BlankLine LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(module-header)
(block
  (binop_equals
    (lc "x")
    (num_literal_big big:<idx:5>)
  )
)
~~~
# FORMATTED
~~~roc
module []

x = 0xFF
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
    (Expr.num_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
; Total type variables: 5
(var #0 _)
(var #1 -> #2)
(var #2 Num *)
(var #3 _)
(var #4 _)
~~~
# TYPES
~~~roc
x : Num(_size)
~~~
