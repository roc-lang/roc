# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
module[}('
)
~~~
# TOKENS
~~~text
KwModule OpenSquare CloseCurly OpenRound MalformedSingleQuoteUnclosed CloseRound ~~~
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

'
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:8 to 1:9

**Parse Error**
at 1:1 to 1:9

**Parse Error**
at 1:10 to 2:1

# CANONICALIZE
~~~clojure
(Expr.block
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag block :type "_a")
~~~
# TYPES
~~~roc
~~~
