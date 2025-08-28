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
(block
  (binop_colon
    (lc "convert")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "a")
            )
            (dot_lc "to_b")
          )
          (lc "a")
        )
      )
      (lc "b")
    )
  )
  (binop_equals
    (lc "convert")
    (lambda
      (body
        (apply_anon
          (binop_pipe
            (lc "a")
            (dot_lc "to_b")
          )
        )
      )
      (args
        (lc "a")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [convert]

convert : a -> b where module(a) | .to_b : a -> b
convert = |a| a.to_b()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
(expr :tag record_access :type "_c")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
