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
(malformed)
~~~
# FORMATTED
~~~roc
inner
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **application_with_whitespace**
This is an unexpected parsing error. Please check your syntax.

**lambda_capture_complex_expressions.md:1:30:1:31:**
```roc
(|outer| |inner| if outer > 0 (outer + inner) else inner)(1)(-2)
```
                             ^


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
; Total type variables: 16
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
(var #6 _)
(var #7 _)
(var #8 _)
(var #9 _)
(var #10 _)
(var #11 _)
(var #12 _)
(var #13 _)
(var #14 _)
(var #15 _)
~~~
# TYPES
~~~roc
~~~
