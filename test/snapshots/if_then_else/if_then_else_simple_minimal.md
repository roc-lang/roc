# META
~~~ini
description=if_then_else (1)
type=expr
~~~
# SOURCE
~~~roc
if bool 1 else 2
~~~
# TOKENS
~~~text
KwIf LowerIdent Int KwElse Int ~~~
# PARSE
~~~clojure
(if_else <0 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

# CANONICALIZE
~~~clojure
(Expr.match)
~~~
# SOLVED
~~~clojure
(expr :tag match :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
