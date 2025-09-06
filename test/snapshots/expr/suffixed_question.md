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
  (binop_dot
    (uc "Stdout")
    (dot_lc "line")
  )
  (malformed)
)
~~~
# FORMATTED
~~~roc
(Stdout..line) ?? ?
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - suffixed_question.md:1:14:1:15
UNDEFINED VARIABLE - suffixed_question.md:1:1:1:12
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **?** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**suffixed_question.md:1:14:1:15:**
```roc
Stdout.line???
```
             ^


# CANONICALIZE
~~~clojure
(Expr.binop_double_question
  (Expr.record_access)
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
