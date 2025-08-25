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

value: GoodAlias
value = "test"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:13

**Unsupported Node**
at 5:1 to 5:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "value")
    (Expr.apply_tag)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
