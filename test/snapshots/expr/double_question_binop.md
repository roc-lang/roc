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
get_name!({  }) ?? "Bob"
~~~
# EXPECTED
NIL
# PROBLEMS
**Unsupported Node**
at 1:15 to 1:17

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
