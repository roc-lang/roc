# META
~~~ini
description=Test error propagation - aliases that reference error types should not propagate errors
type=file
~~~
# SOURCE
~~~roc
module []

BadBase := _

GoodAlias := BadBase

value : GoodAlias
value = "test"
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColonEqual Underscore UpperIdent OpColonEqual UpperIdent LowerIdent OpColon UpperIdent LowerIdent OpAssign String ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "BadBase")
    (underscore)
  )
  (binop_colon_equals
    (uc "GoodAlias")
    (uc "BadBase")
  )
  (binop_colon
    (lc "value")
    (uc "GoodAlias")
  )
  (binop_equals
    (lc "value")
    (str_literal_small "test")
  )
)
~~~
# FORMATTED
~~~roc
module []

BadBase := _
GoodAlias := BadBase
value : GoodAlias
value = "test"
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
value : Str
~~~
