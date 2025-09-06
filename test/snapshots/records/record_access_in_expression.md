# META
~~~ini
description=Record field access in expression
type=expr
~~~
# SOURCE
~~~roc
record.field
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent ~~~
# PARSE
~~~clojure
(binop_dot
  (lc "record")
  (dot_lc "field")
)
~~~
# FORMATTED
~~~roc
record..field
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **record** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_access_in_expression.md:1:1:1:7:**
```roc
record.field
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
