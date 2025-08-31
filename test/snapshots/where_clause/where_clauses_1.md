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
KwModule OpenSquare UpperIdent Comma UpperIdent CloseSquare BlankLine UpperIdent OpenRound LowerIdent Comma LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot UpperIdent BlankLine UpperIdent OpenRound LowerIdent CloseRound OpColon LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon UpperIdent OpenRound UpperIdent CloseRound OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (uc "Hash")

    (uc "Decode")
))
~~~
# FORMATTED
~~~roc
module [Hash, Decode]


Hash((a, hasher)) : (a where module(a).hash : hasher) -> hasher, module(hasher) | Hasher
Decode(a) : a where module(a).decode : List U8 -> a
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
