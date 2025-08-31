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
  (Expr.binop_colon
    (Expr.lookup "x")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "x")
    (Expr.str_literal_big)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
x : Str
~~~
