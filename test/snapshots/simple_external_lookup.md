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
**UNDEFINED VARIABLE**
Nothing is named **List.map** in this scope.
Is there an **import** or **exposing** missing up-top?

**simple_external_lookup.md:1:1:1:9:**
```roc
List.map
```
^^^^^^^^


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
