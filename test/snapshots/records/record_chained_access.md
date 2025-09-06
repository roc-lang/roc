# META
~~~ini
description=Chained record field (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.address.street
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_pipe
  (binop_pipe
    (lc "person")
    (dot_lc "address")
  )
  (dot_lc "street")
)
~~~
# FORMATTED
~~~roc
person.address | .street
~~~
# EXPECTED
UNDEFINED VARIABLE - record_chained_access.md:1:1:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_chained_access.md:1:1:1:7:**
```roc
person.address.street
```
^^^^^^


# CANONICALIZE
~~~clojure
(Expr.record_access)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
