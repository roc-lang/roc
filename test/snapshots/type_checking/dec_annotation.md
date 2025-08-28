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
KwModule OpenSquare CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign Float ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "x")
    (uc "Dec")
  )
  (binop_equals
    (lc "x")
    (frac_literal_big big:<idx:0>)
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
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_a")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
