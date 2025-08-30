# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}|0
as s|||0
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpBar Int KwAs LowerIdent OpOr OpBar Int ~~~
# PARSE
~~~clojure
(module-header
  (exposes
    (malformed malformed:exposed_item_unexpected_token)
))
~~~
# FORMATTED
~~~roc
module [}]

as 
s || 
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:9

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 2:1 to 2:4

**Parse Error**
at 2:9 to 2:9

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
  (Expr.binop_or
    (Expr.lookup "s")
    (Expr.malformed)
  )
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
