# META
~~~ini
description=Error types should propagate through aliases when underscores are used
type=file
~~~
# SOURCE
~~~roc
module []

BadBase := _

BadDerived := BadBase

value : BadDerived
value = "test"

GoodBase := Str

GoodDerived := GoodBase

goodValue : GoodDerived
goodValue = "test"
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColonEqual Underscore UpperIdent OpColonEqual UpperIdent LowerIdent OpColon UpperIdent LowerIdent OpAssign String UpperIdent OpColonEqual UpperIdent UpperIdent OpColonEqual UpperIdent LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "BadBase")
    (underscore)
  )
  (binop_colon_equals
    (uc "BadDerived")
    (uc "BadBase")
  )
  (binop_colon
    (lc "value")
    (uc "BadDerived")
  )
  (binop_equals
    (lc "value")
    (str_literal_small "test")
  )
  (binop_colon_equals
    (uc "GoodBase")
    (uc "Str")
  )
  (binop_colon_equals
    (uc "GoodDerived")
    (uc "GoodBase")
  )
  (binop_colon
    (lc "goodValue")
    (uc "GoodDerived")
  )
  (binop_equals
    (lc "goodValue")
    (str_literal_small "test")
  )
)
~~~
# FORMATTED
~~~roc
module []

BadBase := _
BadDerived := BadBase
value : BadDerived
value = "test"
GoodBase := Str
GoodDerived := GoodBase
goodValue : GoodDerived
goodValue = "test"
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
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
  (Expr.malformed)
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
