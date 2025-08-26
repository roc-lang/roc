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
        (match <25 branches>)
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
module [
	kind,
]

Color : [Red, Green, Blue, Yellow, Orange, Purple]

kind : Color -> Str
kind = \color -> when color is {
	Red
	\Green -> Blue => "primary"
	Yellow.Orange | Purple
	=>
	"secondary"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 6:16 to 6:28

**Parse Error**
at 8:30 to 8:30

**Parse Error**
at 6:16 to 9:2

**Parse Error**
at 9:2 to 9:2

# CANONICALIZE
~~~clojure
(Expr.block
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
