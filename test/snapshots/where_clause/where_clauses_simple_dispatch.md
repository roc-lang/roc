# META
~~~ini
description=Simple where clause with single constraint
type=file
~~~
# SOURCE
~~~roc
module [stringify]

stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# TOKENS
~~~text
KwModule OpenSquare LowerIdent CloseSquare BlankLine LowerIdent OpColon LowerIdent OpArrow UpperIdent KwWhere KwModule OpenRound LowerIdent CloseRound Dot LowerIdent OpColon LowerIdent OpArrow UpperIdent LowerIdent OpAssign OpBar LowerIdent OpBar LowerIdent Dot LowerIdent OpenRound CloseRound ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (lc "stringify")
))
~~~
# FORMATTED
~~~roc
module [stringify]


stringify : a -> Str where module(a).to_str : a -> Str
stringify = |value| value.to_str()
~~~
# EXPECTED
NIL
# PROBLEMS
**UNUSED VARIABLE**
Variable **value** is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_value` to suppress this warning.
The unused variable is declared here:

**where_clauses_simple_dispatch.md:4:21:4:26:**
```roc
stringify = |value| value.to_str()
```
                    ^^^^^


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
