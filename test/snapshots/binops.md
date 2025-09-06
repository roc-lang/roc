# META
~~~ini
description=Binops collection
type=expr
~~~
# SOURCE
~~~roc
(
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
    Bool.True and Bool.False,
    Bool.False or Bool.True,
    None ?? 0,
)
~~~
# TOKENS
~~~text
OpenRound Int OpPlus Int Comma Int OpBinaryMinus Int Comma Int OpStar Int Comma Int OpSlash Int Comma Int MalformedUnknownToken Int Comma Int OpLessThan Int Comma Int OpGreaterThan Int Comma Int OpLessThanOrEq Int Comma Int OpGreaterThanOrEq Int Comma Int OpEquals Int Comma Int OpNotEquals Int Comma Int OpDoubleSlash Int Comma UpperIdent Dot UpperIdent OpAnd UpperIdent Dot UpperIdent Comma UpperIdent Dot UpperIdent OpOr UpperIdent Dot UpperIdent Comma UpperIdent OpDoubleQuestion Int Comma CloseRound ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
% 
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_round_or_comma**
This is an unexpected parsing error. Please check your syntax.



**UNEXPECTED TOKEN IN EXPRESSION**
The token **% ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**binops.md:6:7:6:9:**
```roc
    4 % 2,
```
      ^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 17
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
(var #16 _)
~~~
# TYPES
~~~roc
~~~
