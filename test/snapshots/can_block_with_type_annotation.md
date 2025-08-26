# META
~~~ini
description=Block with type annotation and assignment
type=expr
~~~
# SOURCE
~~~roc
{ x : Str
  x = "hello" }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon UpperIdent LowerIdent OpAssign String CloseCurly ~~~
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
