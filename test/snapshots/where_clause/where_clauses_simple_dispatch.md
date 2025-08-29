# META
~~~ini
description=Simple where clause with single constraint
type=file
~~~
# SOURCE
~~~roc
module [stringify]

stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow UpperIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "stringify")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (lc "a")
          (uc "Str")
        )
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "a")
            )
            (dot_lc "to_str")
          )
          (lc "a")
        )
      )
      (uc "Str")
    )
  )
  (binop_equals
    (lc "stringify")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (lc "value")
            (dot_lc "to_str")
          )
        )
      )
      (args
        (lc "value")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:34 to 3:37

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "stringify")
    (Expr.binop_thin_arrow
      (Expr.binop_colon
        (Expr.binop_thin_arrow
          (Expr.lookup "a")
          (Expr.apply_tag)
        )
        (Expr.binop_colon
          (Expr.lambda)
          (Expr.lookup "a")
        )
      )
      (Expr.apply_tag)
    )
  )
  (Expr.binop_equals
    (Expr.lookup "stringify")
    (Expr.lambda)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
stringify : _b
~~~
