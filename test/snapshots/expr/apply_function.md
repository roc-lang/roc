# META
~~~ini
description=Function application expression
type=expr
~~~
# SOURCE
~~~roc
foo(42, "hello")
~~~
# TOKENS
~~~text
LowerIdent OpenRound Int Comma String CloseRound ~~~
# PARSE
~~~clojure
(apply_lc
  (lc "foo")
  (tuple_literal
    (num_literal_i32 42)
    (str_literal_big "hello")
  )
)
~~~
# FORMATTED
~~~roc
foo((42, "hello"))
~~~
# EXPECTED
UNDEFINED VARIABLE - apply_function.md:1:1:1:4
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named **foo** in this scope.
Is there an **import** or **exposing** missing up-top?

**apply_function.md:1:1:1:4:**
```roc
foo(42, "hello")
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
(var #3 Str)
(var #4 -> #6)
(var #5 _)
(var #6 tuple)
(var #7 fn_pure)
~~~
# TYPES
~~~roc
~~~
