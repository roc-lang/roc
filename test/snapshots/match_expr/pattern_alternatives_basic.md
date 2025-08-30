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
(module-header
  (exposes
    (lc "kind")
))
~~~
# FORMATTED
~~~roc
module [kind]

Color : [Red, Green, Blue, Yellow, Orange, Purple]
kind : Color -> Str
kind = |color| match color
Green
=> "primary"
Yellow
|Orange| Purple => "secondary"
}
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 7:5 to 7:11

**Parse Error**
at 7:24 to 7:27

**Parse Error**
at 9:1 to 9:2

**Unsupported Node**
at 8:23 to 8:29

**Unsupported Node**
at 8:14 to 8:20

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
  (Expr.apply_tag)
  (Expr.malformed)
  (Expr.str_literal_big)
  (Expr.apply_tag)
  (Expr.lambda)
  (Expr.malformed)
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
