# META
~~~ini
description=Record field access with function call
type=expr
~~~
# SOURCE
~~~roc
(person.transform)(42)
~~~
# TOKENS
~~~text
OpenRound LowerIdent Dot LowerIdent CloseRound OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(apply_anon
  (binop_pipe
    (lc "person")
    (dot_lc "transform")
  )
  (num_literal_i32 42)
)
~~~
# FORMATTED
~~~roc
person.transform(42)
~~~
# EXPECTED
NIL
# PROBLEMS
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_access_function_call.md:1:2:1:18:**
```roc
(person.transform)(42)
```
 ^^^^^^^^^^^^^^^^


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
