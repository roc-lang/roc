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
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
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
NO CHANGE
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
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
