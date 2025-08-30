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
(module-header
  (exposes
    (uc "LocalStatus")

    (lc "processColor")
))
~~~
# FORMATTED
~~~roc
module [LocalStatus, processColor]

LocalStatus := [Pending, Complete]
processColor : _ -> LocalStatus
processColor = |color| {
	import Color.RGB
	match color
		RGB | Red => LocalStatus.Pending
		RGB | Green => LocalStatus.Complete
		RGB | Blue => LocalStatus.Pending
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Pattern in Expression Context**
at 5:16 to 5:17

**Unsupported Node**
at 9:5 to 9:21

**Unsupported Node**
at 12:17 to 12:19

**Unsupported Node**
at 13:9 to 13:18

**Unsupported Node**
at 14:18 to 14:20

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.list_literal)
  )
  (Expr.binop_colon
    (Expr.lookup "processColor")
    (Expr.binop_thin_arrow
      (Expr.malformed)
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "processColor")
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
processColor : _a
~~~
