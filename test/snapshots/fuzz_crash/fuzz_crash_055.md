# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[]r:a	where
module(a).h:s
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseSquare LowerIdent OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (lc "r")
    (binop_where
      (lc "a")
      (binop_colon
        (binop_pipe
          (apply_module
            (lc "a")
          )
          (dot_lc "h")
        )
        (lc "s")
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module []

r : a where module(a) | .h : s
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
(expr :tag record_access :type "_b")
~~~
# TYPES
~~~roc
# File does not contain a block of statements
~~~
