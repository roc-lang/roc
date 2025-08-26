# META
~~~ini
description=Example of mixed local and external nominal types in same scope
type=file
~~~
# SOURCE
~~~roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = |color| {

    # bring RGB into scope
    import Color.RGB

    match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
}
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma LowerIdent CloseSquare UpperIdent OpColonEqual OpenSquare UpperIdent Comma UpperIdent CloseSquare LowerIdent OpColon Underscore OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar OpenCurly KwImport UpperIdent Dot UpperIdent KwMatch LowerIdent OpenCurly UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent UpperIdent Dot UpperIdent OpFatArrow UpperIdent Dot UpperIdent CloseCurly CloseCurly ~~~
# PARSE
~~~clojure
(block
  (binop_colon_equals
    (uc "LocalStatus")
    (list_literal
      (uc "Pending")
      (uc "Complete")
    )
  )
  (binop_colon
    (lc "processColor")
    (binop_thin_arrow
      (underscore)
      (uc "LocalStatus")
    )
  )
  (binop_equals
    (lc "processColor")
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
	LocalStatus,
	processColor,
]

LocalStatus := [Pending, Complete]

processColor : _ -> LocalStatus
processColor = \color -> {
	import Color.RGB
	

match color {
        RGB.Red => LocalStatus.Pending
        RGB.Green => LocalStatus.Complete
        RGB.Blue => LocalStatus.Pending
    }
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 11:5 to 11:17

**Parse Error**
at 12:17 to 12:17

**Parse Error**
at 13:19 to 13:19

**Parse Error**
at 14:18 to 14:18

**Parse Error**
at 11:5 to 16:1

**Parse Error**
at 16:1 to 16:1

**Parse Error**
at 6:24 to 16:2

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
