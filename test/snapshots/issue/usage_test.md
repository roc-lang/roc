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
module []

UnusedType := _
UsedType := _
value : UsedType
value = 42
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
