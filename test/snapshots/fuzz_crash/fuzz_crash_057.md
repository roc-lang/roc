# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]s:b->c where module(a).t:c,u:o...
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent Comma LowerIdent OpColon LowerIdent TripleDot ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "s")
    (binop_colon
      (tuple_literal
        (binop_where
          (binop_thin_arrow
            (lc "b")
            (lc "c")
          )
          (binop_colon
            (binop_pipe
              (apply_module
                (lc "a")
              )
              (dot_lc "t")
            )
            (lc "c")
          )
        )
        (lc "u")
      )
      (lc "o")
    )
  )
  (ellipsis)
)
~~~
# FORMATTED
~~~roc
module []

s : (b -> c where module(a).t : c, u) : o
...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:28 to 1:31

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "s")
    (Expr.binop_colon
      (Expr.tuple_literal)
      (Expr.lookup "o")
    )
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
