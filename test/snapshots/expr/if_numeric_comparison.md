# META
~~~ini
description=If expression with numeric comparison
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 1 else 2
~~~
# TOKENS
~~~text
KwIf Int OpGreaterThan Int Int KwElse Int ~~~
# PARSE
~~~clojure
(if_else <2 branches>)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
**Parse Error**
at 1:1 to 1:10

# CANONICALIZE
~~~clojure
(Expr.unary_neg)
~~~
# SOLVED
~~~clojure
(expr :tag unary_neg :type "Num(_a)")
~~~
# TYPES
~~~roc
Num(_a)
~~~
