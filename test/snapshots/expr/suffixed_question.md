# META
~~~ini
description=suffixed_question
type=expr
~~~
# SOURCE
~~~roc
Stdout.line???
~~~
# TOKENS
~~~text
UpperIdent Dot LowerIdent OpDoubleQuestion OpQuestion ~~~
# PARSE
~~~clojure
(binop_double_question
  (binop_pipe
    (uc "Stdout")
    (dot_lc "line")
  )
  (malformed malformed:expr_unexpected_token)
)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - suffixed_question.md:1:14:1:15
UNDEFINED VARIABLE - suffixed_question.md:1:1:1:12
# PROBLEMS
**Parse Error**
at 1:14 to 1:14

**Unsupported Node**
at 1:1 to 1:14

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
