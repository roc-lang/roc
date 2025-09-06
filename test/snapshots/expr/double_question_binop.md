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
UNDEFINED VARIABLE - double_question_binop.md:1:1:1:10
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
  (Expr.fn_call)
  (Expr.str_literal_small)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 Str)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
