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
**Pattern in Expression Context**
at 3:12 to 3:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "value")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "value")
    (Expr.str_literal_small)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.apply_tag)
  )
  (Expr.binop_colon
    (Expr.lookup "goodValue")
    (Expr.apply_tag)
  )
  (Expr.binop_equals
    (Expr.lookup "goodValue")
    (Expr.str_literal_small)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
value : Str
goodValue : Str
~~~
