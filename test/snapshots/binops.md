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
(expr :tag malformed :type "Error")
~~~
# TYPES
~~~roc
Error
~~~
