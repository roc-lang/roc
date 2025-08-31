# META
~~~ini
description=Module dispatch in where clause
type=file
~~~
# SOURCE
~~~roc
module [deserialize]

deserialize : List(U8) -> Result(a, [DecodeErr])
	where module(a).decode : List(U8) -> Result(a, [DecodeErr])
deserialize = |_| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow UpperIdent OpenRound LowerIdent Comma OpenSquare UpperIdent CloseSquare CloseRound LowerIdent OpAssign OpBar Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "deserialize")
))
~~~
# FORMATTED
~~~roc
module [deserialize]


deserialize : List U8 -> Result(a, [DecodeErr]) where module(a).decode : List U8 -> Result(a, [DecodeErr])
deserialize = |_| ...
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
