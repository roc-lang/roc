# META
~~~ini
description=Field access expression simple expression
type=expr
~~~
# SOURCE
~~~roc
person.name
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_dot
  (lc "person")
  (dot_lc "name")
)
~~~
# FORMATTED
~~~roc
person..name
~~~
# EXPECTED
UNDEFINED VARIABLE - field_access.md:1:1:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**field_access.md:1:1:1:7:**
```roc
person.name
```
^^^^^^


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
