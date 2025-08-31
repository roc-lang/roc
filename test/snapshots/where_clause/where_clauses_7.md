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
KwModule OpenSquare UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound LineComment OpColon LineComment LowerIdent LineComment KwWhere LineComment KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent LineComment OpArrow LineComment LowerIdent Comma LineComment KwModule OpenRound LowerIdent CloseRound Dot UpperIdent BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpenRound LineComment UpperIdent OpenRound UpperIdent CloseRound Comma LineComment CloseRound OpArrow LowerIdent ~~~
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


# After header
Hash((a, hasher)) : # After colon
	(a where # After var
	# After where
module(a).hash : hasher) -> # After method
	# After arrow
hasher, # After first clause
	module(hasher) | Hasher,

Decode(a) : a where module(a).decode( # After method args open
List(U8)) -> # After method arg
a
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
