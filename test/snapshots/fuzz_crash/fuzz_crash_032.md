# META
~~~ini
description=fuzz crash, unterminated single quote
type=file
~~~
# SOURCE
~~~roc
module [tus,r]

LocalStatus :lue => Loc= [Pending, Complete]

olor : _ -> tus
olor = |color| { import Color.RGB

    match color { RGB => LocalStatus.Pending
Green => LocalStatus-Complete
  B.Blue => LocalStatus.Pending
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent CloseSquare UpperIdent OpColon LowerIdent OpFatArrow UpperIdent OpAssign OpenSquare UpperIdent Comma UpperIdent CloseSquare LowerIdent OpColon Underscore OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwImport UpperIdent Dot UpperIdent KwMatch LowerIdent OpenCurly UpperIdent OpFatArrow UpperIdent Dot UpperIdent UpperIdent OpFatArrow UpperIdent OpUnaryMinus UpperIdent UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (uc "LocalStatus")
    (binop_thick_arrow
      (lc "lue")
      (binop_equals
        (uc "Loc")
        (list_literal
          (uc "Pending")
          (uc "Complete")
        )
      )
    )
  )
  (binop_colon
    (lc "olor")
    (binop_thin_arrow
      (underscore)
      (lc "tus")
    )
  )
  (binop_equals
    (lc "olor")
    (lambda
      (body
        (block
          (import
            (uc "Color")
            (uc "RGB")
          )
          (match <33 branches>)
        )
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
	tus,
	r,
]

LocalStatus : lue => Loc = [Pending, Complete]

olor : _ -> tus
olor = \color -> {
	import Color.RGB
	

when color is {
		RGB
		=>
		LocalStatus.Pending
		Green
		=>
		LocalStatus - Complete
		B.Blue
		=>
		LocalStatus.Pending
	}
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 8:5 to 8:17

**Parse Error**
at 8:23 to 8:23

**Parse Error**
at 9:7 to 9:7

**Parse Error**
at 10:10 to 10:10

**Parse Error**
at 8:5 to 12:1

**Parse Error**
at 12:1 to 12:1

**Parse Error**
at 6:16 to 12:2

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
