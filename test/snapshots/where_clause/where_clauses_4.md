# META
~~~ini
description=where_clauses (4)
type=file
~~~
# SOURCE
~~~roc
module [decodeThings]

import Decode exposing [Decode]

decodeThings : List(List(U8)) -> List(a)
	where module(a).Decode
decodeThings = ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(block
  (import
    (binop_exposing
      (uc "Decode")
      (list_literal
        (uc "Decode")
      )
    )
  )
  (binop_colon
    (lc "decodeThings")
    (binop_where
      (binop_thin_arrow
        (apply_uc
          (uc "List")
          (apply_uc
            (uc "List")
            (uc "U8")
          )
        )
        (apply_uc
          (uc "List")
          (lc "a")
        )
      )
      (binop_pipe
        (apply_module
          (lc "a")
        )
        (uc "Decode")
      )
    )
  )
  (binop_equals
    (lc "decodeThings")
    (ellipsis)
  )
)
~~~
# FORMATTED
~~~roc
module [decodeThings]

import Decode exposing [Decode]
decodeThings : List List U8 -> List a where module(a) | Decode
decodeThings = ...
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 3:1 to 3:32

**Unsupported Node**
at 6:14 to 6:17

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
  (Expr.binop_equals
    (Expr.lookup "decodeThings")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_b")
~~~
# TYPES
~~~roc
decodeThings : Error
~~~
