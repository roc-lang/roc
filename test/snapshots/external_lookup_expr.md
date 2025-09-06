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
(binop_pipe
  (uc "Json")
  (dot_lc "utf8")
)
~~~
# FORMATTED
~~~roc
Json.utf8
~~~
# EXPECTED
UNDEFINED VARIABLE - external_lookup_expr.md:1:1:1:10
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **Json.utf8** in this scope.
Is there an **import** or **exposing** missing up-top?

**external_lookup_expr.md:1:1:1:10:**
```roc
Json.utf8
```
^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.module_access
  (Expr.lookup "Json")
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
