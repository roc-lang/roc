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
(binop_dot
  (binop_dot
    (lc "person")
    (dot_lc "address")
  )
  (dot_lc "street")
)
~~~
# FORMATTED
~~~roc
person..address..street
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
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
