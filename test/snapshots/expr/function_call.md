# META
~~~ini
description=Function call expression
type=expr
~~~
# SOURCE
~~~roc
add(5, 3)
~~~
# TOKENS
~~~text
LowerIdent OpenRound Int Comma Int CloseRound ~~~
# PARSE
~~~clojure
(apply_lc
  (lc "add")
  (tuple_literal
    (num_literal_i32 5)
    (num_literal_i32 3)
  )
)
~~~
# FORMATTED
~~~roc
add((5, 3))
~~~
# EXPECTED
UNDEFINED VARIABLE - function_call.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **add** in this scope.
Is there an **import** or **exposing** missing up-top?

**function_call.md:1:1:1:4:**
```roc
add(5, 3)
```
^^^


# CANONICALIZE
~~~clojure
(Expr.fn_call)
~~~
# SOLVED
~~~clojure
; Total type variables: 8
(var #0 _)
(var #1 -> #7)
(var #2 Num *)
(var #3 Num *)
(var #4 -> #6)
(var #5 _)
(var #6 tuple)
(var #7 fn_pure)
~~~
# TYPES
~~~roc
~~~
