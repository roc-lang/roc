# META
~~~ini
description=Simple external declaration lookup
type=expr
~~~
# SOURCE
~~~roc
List.map
~~~
# TOKENS
~~~text
UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_pipe
  (uc "List")
  (dot_lc "map")
)
~~~
# FORMATTED
~~~roc
List.map
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.module_access
  (Expr.malformed)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag module_access :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
