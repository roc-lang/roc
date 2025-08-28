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
