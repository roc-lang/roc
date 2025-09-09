# META
~~~ini
description=External declaration lookup expression
type=expr
~~~
# SOURCE
~~~roc
Json.utf8
~~~
# TOKENS
~~~text
UpperIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_dot
  (uc "Json")
  (dot_lc "utf8")
)
~~~
# FORMATTED
~~~roc
Json..utf8
~~~
# EXPECTED
UNDEFINED VARIABLE - external_lookup_expr.md:1:1:1:10
# PROBLEMS
NIL
# CANONICALIZE
~~~clojure
(Expr.record_access)
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
