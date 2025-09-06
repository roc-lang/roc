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
UNDEFINED VARIABLE - record_access_function_call.md:1:2:1:8
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**record_access_function_call.md:1:18:1:19:**
```roc
(person.transform)(42)
```
                 ^


**UNDEFINED VARIABLE**
Nothing is named **person** in this scope.
Is there an **import** or **exposing** missing up-top?

**record_access_function_call.md:1:2:1:8:**
```roc
(person.transform)(42)
```
 ^^^^^^


# CANONICALIZE
~~~clojure
(Expr.apply_ident)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
