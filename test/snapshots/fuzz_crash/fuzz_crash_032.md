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
(module-header
  (exposes
    (lc "tus")

    (lc "r")
))
~~~
# FORMATTED
~~~roc
module [tus, r]

LocalStatus : lue => Loc = [Pending, Complete]
olor : _ -> tus
olor = |color| {
	import Color.RGB
	match color
		RGB => LocalStatus.Pending
		Green => LocalStatus - Complete
		B | Blue => LocalStatus.Pending
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 5:8 to 5:9

**Unsupported Node**
at 6:18 to 6:34

**Unsupported Node**
at 8:23 to 8:25

**Unsupported Node**
at 9:1 to 9:6

**Unsupported Node**
at 10:10 to 10:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_thick_arrow
      (Expr.malformed)
      (Expr.binop_equals
        (Expr.apply_tag)
        (Expr.list_literal)
      )
    )
  )
  (Expr.binop_colon
    (Expr.lookup "olor")
    (Expr.binop_thin_arrow
      (Expr.malformed)
      (Expr.lookup "tus")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "olor")
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
olor : _a
~~~
