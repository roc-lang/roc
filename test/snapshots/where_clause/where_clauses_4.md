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
KwModule OpenSquare LowerIdent CloseSquare BlankLine KwImport UpperIdent KwExposing OpenSquare UpperIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent OpenRound UpperIdent CloseRound CloseRound OpArrow UpperIdent OpenRound LowerIdent CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot UpperIdent LowerIdent OpAssign TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "decodeThings")
))
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
(Expr.block
  (Expr.malformed)
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
