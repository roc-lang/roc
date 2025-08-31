# META
~~~ini
description=Multiple where constraints on different type variables
type=file
~~~
# SOURCE
~~~roc
module [process]

process : a, b -> c where module(a).convert : a -> c, module(b).transform : b -> c
process = |_, _| ...
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent Comma KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent LowerIdent OpAssign OpBar Underscore Comma Underscore OpBar TripleDot ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "process")
))
~~~
# FORMATTED
~~~roc
module [process]


process : a -> b -> (c where module(a).convert : a) -> c, module(b).transform : b -> c
process = |_, _| ...
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
(expr :tag block :type "_d")
~~~
# TYPES
~~~roc
~~~
