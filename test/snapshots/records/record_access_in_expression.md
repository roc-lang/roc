# META
~~~ini
description=Record field access used in expressions (dot-access)
type=expr
~~~
# SOURCE
~~~roc
person.age + 5
~~~
# TOKENS
~~~text
LowerIdent Dot LowerIdent OpPlus Int ~~~
# PARSE
~~~clojure
(binop_plus
  (binop_pipe
    (lc "person")
    (dot_lc "age")
  )
  (num_literal_i32 5)
)
~~~
# FORMATTED
~~~roc
person.age + 5
~~~
# EXPECTED
UNDEFINED VARIABLE - record_access_in_expression.md:1:1:1:7
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_access_in_expression.md:1:1:1:7:**
```roc
person.age + 5
```
^^^^^^


# CANONICALIZE
~~~clojure
(Expr.binop_plus
  (Expr.binop_pipe)
  (Expr.num_literal_i32 5)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 -> #4)
(var #4 -> #5)
(var #5 Num *)
~~~
# TYPES
~~~roc
~~~
