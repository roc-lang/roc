# META
~~~ini
description=Example of a simple nominal tag union
type=file
~~~
# SOURCE
~~~roc
module [Color, blue]

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue

yellow : Color
yellow = Color.Yellow
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent LowerIdent OpColon UpperIdent LowerIdent OpAssign UpperIdent Dot UpperIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Green")
      (uc "Blue")
    )
  )
  (binop_colon
    (lc "blue")
    (uc "Color")
  )
  (binop_equals
    (lc "blue")
    (binop_pipe
      (uc "Color")
      (uc "Blue")
    )
  )
  (binop_colon
    (lc "yellow")
    (uc "Color")
  )
  (binop_equals
    (lc "yellow")
    (binop_pipe
      (uc "Color")
      (uc "Yellow")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	Color,
	blue,
]

Color := [Red, Green, Blue]

blue : Color
blue = Color.Blue
yellow : Color
yellow = Color.Yellow
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
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
