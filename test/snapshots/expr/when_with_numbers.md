# META
~~~ini
description=When is old syntax use match instead (should error)
type=expr
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 3 -> 4
~~~
# TOKENS
~~~text
LowerIdent LowerIdent LowerIdent Int OpArrow Int Int OpArrow Int ~~~
# PARSE
~~~clojure
(lc "when")
~~~
# FORMATTED
~~~roc
when
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.lookup "when")
~~~
# SOLVED
~~~clojure
(expr :tag lookup :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
