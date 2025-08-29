# META
~~~ini
description=If expression with True boolean literal
type=expr
~~~
# SOURCE
~~~roc
if True 1 else 2
~~~
# TOKENS
~~~text
KwIf UpperIdent Int KwElse Int ~~~
# PARSE
~~~clojure
(if_else <0 branches>)
~~~
# FORMATTED
~~~roc
if True 1 else 2
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
