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
      (tuple_literal
        (uc "Red")
        (uc "Green")
        (uc "Blue")
      )
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
NO CHANGE
~~~
# EXPECTED
INVALID NOMINAL TAG - nominal_tag_simple.md:9:10:9:22
# PROBLEMS
**Unsupported Node**
at 3:1 to 4:1

**Pattern in Expression Context**
at 5:8 to 5:13

**Unsupported Node**
at 6:8 to 6:13

**Pattern in Expression Context**
at 6:13 to 6:17

**Pattern in Expression Context**
at 8:10 to 8:15

**Unsupported Node**
at 9:10 to 9:15

**Pattern in Expression Context**
at 9:15 to 9:21

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "blue")
    (Expr.malformed)
  )
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "yellow")
    (Expr.malformed)
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
