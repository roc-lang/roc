# META
~~~ini
description=Error cases for where clauses
type=file
~~~
# SOURCE
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]

# Missing colon in constraint
broken_fn1 : a -> b
  where
    module(a).method -> b

# Empty where clause
broken_fn2 : a -> b
  where

# Referencing undefined type variable
broken_fn3 : a -> b
  where
    module(c).method : c -> d
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent Comma LowerIdent Comma LowerIdent CloseSquare BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpArrow LowerIdent BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere BlankLine LineComment LowerIdent OpColon LowerIdent OpArrow LowerIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "broken_fn1")

    (lc "broken_fn2")

    (lc "broken_fn3")
))
~~~
# FORMATTED
~~~roc
module [broken_fn1, broken_fn2, broken_fn3]


# Missing colon in constraint
broken_fn1 : a -> b where module(a).method -> b
# Empty where clause
broken_fn2 : a -> b where 
# Referencing undefined type variable
broken_fn3 : a -> b where module(c).method : c -> d
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
(expr :tag block :type "_e")
~~~
# TYPES
~~~roc
~~~
