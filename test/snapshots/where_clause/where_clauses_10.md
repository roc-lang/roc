# META
~~~ini
description=where_clauses (10)
type=file
~~~
# SOURCE
~~~roc
module [decode]

import Decode exposing [Decode]

decodeThings # After member name
	: # After colon
		List(List(U8)) -> List(a) # After anno
			where # after where
				module(a).Decode
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "decode")
))
~~~
# FORMATTED
~~~roc
module [decode]

import Decode exposing [Decode]
decodeThings : List List U8 -> List a where module(a) | Decode
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:32

**Unsupported Node**
at 9:11 to 9:14

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_colon
    (Expr.lookup "decodeThings")
    (Expr.binop_colon
      (Expr.binop_thin_arrow
        (Expr.apply_tag)
        (Expr.apply_tag)
      )
      (Expr.lambda)
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
