# META
~~~ini
description=Simple qualified tag test
type=file
~~~
# SOURCE
~~~roc
module [Color]

Color := [Red, Blue]

test = Color.Red
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Blue")
    )
  )
  (binop_equals
    (lc "test")
    (binop_pipe
      (uc "Color")
      (uc "Red")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Color]

Color := [Red, Blue]
test = Color.Red
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
