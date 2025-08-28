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
module [Hash]

# After header
Hash((a, hasher)) : # After method
((a where module(a) | .hash : hasher) -> # After arrow
hasher, module(hasher) | Hasher)
Decode(a) : a where module(a) | .decode( # After method args open
List(U8) -> ( # After method arg
 -> a))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 16:3 to 16:3

**Parse Error**
at 14:9 to 16:9

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
