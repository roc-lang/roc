# META
~~~ini
description=Basic pattern alternatives with multiple tag patterns
type=file
~~~
# SOURCE
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = |color| match color {
    Red | Green | Blue => "primary"
    Yellow | Orange | Purple => "secondary"
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare UpperIdent OpColon OpenSquare UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent Comma UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar KwMatch LowerIdent OpenCurly UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow String UpperIdent OpBar UpperIdent OpBar UpperIdent OpFatArrow String CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "Color")
    (list_literal
      (uc "Red")
      (uc "Green")
      (uc "Blue")
      (uc "Yellow")
      (uc "Orange")
      (uc "Purple")
    )
  )
  (binop_colon
    (lc "kind")
    (binop_thin_arrow
      (uc "Color")
      (uc "Str")
    )
  )
  (binop_equals
    (lc "kind")
    (lambda
      (body
        (match
          (scrutinee             (lc "color")
))
      )
      (args
        (lc "color")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]
kind : Color -> Str
kind = |color| match color
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:30 to 8:33

**Parse Error**
at 6:28 to 9:2

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.lookup "kind")
    (Expr.binop_thin_arrow
      (Expr.apply_tag)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "kind")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
kind : _a
~~~
