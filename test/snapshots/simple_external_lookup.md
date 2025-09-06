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
UNDEFINED VARIABLE - simple_external_lookup.md:1:1:1:9
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.module_access
  (Expr.lookup "List")
  (Expr.record_accessor)
)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
