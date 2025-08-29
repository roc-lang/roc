# META
~~~ini
description=Minimal where clause test
type=file
~~~
# SOURCE
~~~roc
module [convert_me]

convert_me : a -> b
	where
		module(a).convert : a -> b
convert_me = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "convert_me")
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
            (dot_lc "convert")
          )
          (lc "a")
        )
      )
      (lc "b")
    )
  )
  (binop_equals
    (lc "convert_me")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [convert_me]

convert_me : a -> b where module(a).convert : a -> b
convert_me = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 5:9 to 5:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "convert_me")
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
    (Expr.lookup "convert_me")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_c")
~~~
# TYPES
~~~roc
convert_me : Error
~~~
