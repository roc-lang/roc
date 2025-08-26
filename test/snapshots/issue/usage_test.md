# META
~~~ini
description=Test if usage affects error type conversion
type=file
~~~
# SOURCE
~~~roc
module []

UnusedType := _

UsedType := _

value : UsedType
value = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColonEqual Underscore UpperIdent OpColonEqual Underscore LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "UnusedType")
    (underscore)
  )
  (binop_colon_equals
    (uc "UsedType")
    (underscore)
  )
  (binop_colon
    (lc "value")
    (uc "UsedType")
  )
  (binop_equals
    (lc "value")
    (num_literal_i32 42)
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
