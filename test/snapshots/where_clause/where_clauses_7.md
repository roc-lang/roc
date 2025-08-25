# META
~~~ini
description=where_clauses (7)
type=file
~~~
# SOURCE
~~~roc
module [Hash]

Hash(a, hasher) # After header
	: # After colon
		a # After var
			where # After where
				module(a).hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				module(hasher).Hasher

Decode(a) : a
	where
		module(a).decode( # After method args open
			List(U8), # After method arg
		) -> a
~~~
# TOKENS
~~~text
KwModule OpenSquare UpperIdent CloseSquare UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound Comma CloseRound OpArrow LowerIdent ~~~
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
    (binop_where
      (lc "a")
      (apply_anon
        (binop_pipe
          (apply_module
            (lc "a")
          )
          (dot_lc "decode")
        )
        (binop_thin_arrow
          (apply_uc
            (uc "List")
            (uc "U8")
          )
          (binop_thin_arrow
            (malformed malformed:expr_unexpected_token)
            (lc "a")
          )
        )
      )
    )
  )
)
~~~
# FORMATTED
~~~roc
module [
	Hash,
]

Hash((a, hasher)) # After header: (
	(a # After var where module(a) | .hash: hasher # After method) # After method -> hasher,	# After first clause
	module(hasher) | Hasher
)Decode(a): (a where module(a) | .decode(List(U8) -> () -> a)))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 16:3 to 16:3

**Parse Error**
at 14:12 to 16:9

**Unsupported Node**
at 1:1 to 1:1

**Unsupported Node**
at 1:1 to 1:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "[]_others")
~~~
# TYPES
~~~roc
~~~
