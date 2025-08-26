# META
~~~ini
description=Error cases for where clauses
type=file
~~~
# SOURCE
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]

# Missing colon in constraint
broken_fn1 : a -> b
  where
    module(a).method -> b

# Empty where clause
broken_fn2 : a -> b
  where

# Referencing undefined type variable
broken_fn3 : a -> b
  where
    module(c).method : c -> d
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpArrow LowerIdent LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "broken_fn1")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (lc "a")
          (lc "b")
        )
        (binop_pipe
          (apply_module
            (lc "a")
          )
          (dot_lc "method")
        )
      )
      (lc "b")
    )
  )
  (binop_colon
    (lc "broken_fn2")
    (binop_thin_arrow
      (binop_where
        (binop_thin_arrow
          (binop_where
            (binop_thin_arrow
              (lc "a")
              (lc "b")
            )
            (binop_colon
              (lc "broken_fn3")
              (lc "a")
            )
          )
          (lc "b")
        )
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "c")
            )
            (dot_lc "method")
          )
          (lc "c")
        )
      )
      (lc "d")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	broken_fn1,
	broken_fn2,
	broken_fn3,
]

broken_fn1 : a -> b where module(a) | .method -> b

# Empty where clause
broken_fn2 : a -> b where broken_fn3 : a -> b where module(c) | .method : c -> d
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
~~~
