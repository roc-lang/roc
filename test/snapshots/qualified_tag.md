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
      (tuple_literal
        (uc "Red")
        (uc "Blue")
      )
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
module [
	Color,
]

Color := [(Red, Blue)]
test = Color.Red
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 4:1

**Unsupported Node**
at 5:8 to 5:13

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
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
