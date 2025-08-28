# META
~~~ini
description=Simple test for single underscore type becoming error type
type=file
~~~
# SOURCE
~~~roc
module []

BadType := _

foo : BadType
foo = 42
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare UpperIdent OpColonEqual Underscore LowerIdent OpColon UpperIdent LowerIdent OpAssign Int ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "BadType")
    (underscore)
  )
  (binop_colon
    (lc "foo")
    (uc "BadType")
  )
  (binop_equals
    (lc "foo")
    (num_literal_i32 42)
  )
)
~~~
# FORMATTED
~~~roc
module []

BadType := _
foo : BadType
foo = 42
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
