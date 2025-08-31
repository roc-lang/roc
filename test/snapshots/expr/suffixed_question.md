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
Stdout.line ?? ?
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**suffixed_question.md:1:14:1:15:**
```roc
Stdout.line???
```
             ^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**suffixed_question.md:1:1:1:15:**
```roc
Stdout.line???
```
^^^^^^^^^^^^^^


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
