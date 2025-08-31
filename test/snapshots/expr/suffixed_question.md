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
  (malformed)
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


**UNDEFINED VARIABLE**
Nothing is named **Stdout.line** in this scope.
Is there an **import** or **exposing** missing up-top?

**suffixed_question.md:1:1:1:12:**
```roc
Stdout.line???
```
^^^^^^^^^^^


# CANONICALIZE
~~~clojure
(Expr.binop_double_question
  (Expr.module_access
    (Expr.malformed)
    (Expr.malformed)
  )
  (Expr.malformed)
)
~~~
# SOLVED
~~~clojure
(expr :tag binop_double_question :type "_a")
~~~
# TYPES
~~~roc
_a
~~~
