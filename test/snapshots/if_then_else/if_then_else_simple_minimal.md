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
UNDEFINED VARIABLE - if_then_else_simple_minimal.md:1:4:1:8
# PROBLEMS
**Parse Error**
at 1:1 to 1:9

# CANONICALIZE
~~~clojure
(Expr.if_else)
~~~
# SOLVED
~~~clojure
(expr :tag if_else :type "Num(_size)")
~~~
# TYPES
~~~roc
Num(_size)
~~~
