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
(module-header
  (exposes
    (uc "Hash")
))
~~~
# FORMATTED
~~~roc
module [Hash]

Hash((a, hasher)) : 
	(a where module(a).hash : hasher) -> hasher,
	module(hasher) | Hasher,
Decode(a) : a where module(a).decode(List(U8) -> ()  -> a))
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 16:3 to 16:5

**Parse Error**
at 14:9 to 16:9

**Unsupported Node**
at 7:11 to 7:14

**Unsupported Node**
at 10:11 to 10:19

**Unsupported Node**
at 14:9 to 14:12

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.tuple_literal
      (Expr.binop_thin_arrow
        (Expr.binop_colon
          (Expr.lookup "a")
          (Expr.binop_colon
            (Expr.lambda)
            (Expr.lookup "hasher")
          )
        )
        (Expr.lookup "hasher")
      )
      (Expr.lambda)
    )
  )
  (Expr.binop_colon
    (Expr.apply_tag)
    (Expr.binop_colon
      (Expr.lookup "a")
      (Expr.apply_ident)
    )
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
~~~
