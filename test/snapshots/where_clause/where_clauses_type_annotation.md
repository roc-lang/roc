# META
~~~ini
description=Simple type annotation with where clause
type=file
~~~
# SOURCE
~~~roc
module [convert]

convert : a -> b where module(a).to_b : a -> b
convert = |a| a.to_b()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "convert")
))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:30 to 3:33

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "convert")
    (Expr.binop_thin_arrow
      (Expr.binop_colon
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.lookup "b")
        )
        (Expr.binop_colon
          (Expr.lambda)
          (Expr.lookup "a")
        )
      )
      (Expr.lookup "b")
    )
  )
  (Expr.binop_equals
    (Expr.lookup "convert")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
convert : _c
~~~
