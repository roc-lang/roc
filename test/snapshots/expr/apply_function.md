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
NIL
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
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
(expr :tag apply_ident :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
