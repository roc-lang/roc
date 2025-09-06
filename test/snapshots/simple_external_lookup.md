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
(Expr.binop_pipe)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
