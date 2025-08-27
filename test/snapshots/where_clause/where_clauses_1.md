# META
~~~ini
description=where_clauses (1)
type=file
~~~
# SOURCE
~~~roc
module [Hash, Decode]

Hash(a, hasher) : a
	where
		module(a).hash : hasher -> hasher,
		module(hasher).Hasher

Decode(a) : a where module(a).decode : List(U8) -> a
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(block
  (binop_colon
    (apply_uc
      (uc "Hash")
      (tuple_literal
        (lc "a")
        (lc "hasher")
      )
    )
    (tuple_literal
      (binop_thin_arrow
        (binop_where
          (lc "a")
          (binop_colon
            (binop_pipe
              (apply_module
                (lc "a")
              )
              (dot_lc "hash")
            )
            (lc "hasher")
          )
        )
        (lc "hasher")
      )
      (binop_pipe
        (apply_module
          (lc "hasher")
        )
        (uc "Hasher")
      )
    )
  )
  (binop_colon
    (apply_uc
      (uc "Decode")
      (lc "a")
    )
    (binop_thin_arrow
      (binop_where
        (lc "a")
        (binop_colon
          (binop_pipe
            (apply_module
              (lc "a")
            )
            (dot_lc "decode")
          )
          (apply_uc
            (uc "List")
            (uc "U8")
          )
        )
      )
      (lc "a")
    )
  )
)
~~~
# FORMATTED
~~~roc
module [Hash, Decode]

Hash((a, hasher)) : ((a where module(a) | .hash : hasher) -> hasher, module(hasher) | Hasher)

Decode(a) : a where module(a) | .decode : List U8 -> a
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
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
