# META
~~~ini
description=Multiple where constraints on different type variables
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "process")
    (binop_thin_arrow
      (lc "a")
      (binop_thin_arrow
        (lc "b")
        (binop_thin_arrow
          (binop_colon
            (tuple_literal
              (binop_thin_arrow
                (binop_where
                  (lc "c")
                  (binop_colon
                    (binop_pipe
                      (apply_module
                        (lc "a")
                      )
                      (dot_lc "convert")
                    )
                    (lc "a")
                  )
                )
                (lc "c")
              )
              (binop_pipe
                (apply_module
                  (lc "b")
                )
                (dot_lc "transform")
              )
            )
            (lc "b")
          )
          (lc "c")
        )
      )
    )
  )
  (binop_equals
    (lc "process")
    (lambda
      (body
        (ellipsis)
      )
      (args
        (tuple_literal
          (underscore)
          (underscore)
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [process]

process : a -> b -> ((c where module(a) | .convert : a) -> c, module(b) | .transform) : b -> c
process = |_, _| ...
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
(expr :tag record_access :type "_d")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
