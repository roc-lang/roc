# META
~~~ini
description=Complex expressions with captures - lambda with conditionals and captures
type=expr
~~~
# SOURCE
~~~roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
~~~
# TOKENS
~~~text
OpenRound OpBar LowerIdent OpBar OpBar LowerIdent OpBar KwIf LowerIdent OpGreaterThan Int OpenRound LowerIdent OpPlus LowerIdent CloseRound KwElse LowerIdent CloseRound OpenRound Int CloseRound OpenRound OpUnaryMinus Int CloseRound ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
inner
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **else ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**lambda_capture_complex_expressions.md:1:47:1:52:**
```roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
```
                                              ^^^^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **inner** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**lambda_capture_complex_expressions.md:1:52:1:57:**
```roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
```
                                                   ^^^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
