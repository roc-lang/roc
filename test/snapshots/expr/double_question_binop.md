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
**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**double_question_binop.md:1:1:1:23:**
```roc
get_name!({}) ?? "Bob"
```
^^^^^^^^^^^^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
