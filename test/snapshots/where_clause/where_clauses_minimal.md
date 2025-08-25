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
module [
	convert_me,
]

convert_me: ((a -> b where a() | .convert: a) -> b)
convert_me = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:14 to 5:29

**Unsupported Node**
at 6:14 to 6:17

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.lookup "convert_me")
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "Error")
~~~
# TYPES
~~~roc
~~~
