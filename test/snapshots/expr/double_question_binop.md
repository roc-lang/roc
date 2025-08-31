# META
~~~ini
description=Double question default value
type=expr
~~~
# SOURCE
~~~roc
get_name!({}) ?? "Bob"
~~~
# TOKENS
~~~text
LowerIdent OpBang OpenRound OpenCurly CloseCurly CloseRound OpDoubleQuestion String ~~~
# PARSE
~~~clojure
(binop_double_question
  (apply_anon
    (not_lc "get_name")
    (record_literal)
  )
  (str_literal_small "Bob")
)
~~~
# FORMATTED
~~~roc
get_name!({}) ?? "Bob"
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **get_name!** in this scope.
Is there an **import** or **exposing** missing up-top?

**double_question_binop.md:1:1:1:10:**
```roc
get_name!({}) ?? "Bob"
```
^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.binop_double_question
  (Expr.apply_ident)
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_question :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
