# META
~~~ini
description=Simple when expression
type=expr
~~~
# SOURCE
~~~roc
when x is
    Ok(value) -> value
    Err(msg) -> msg
~~~
# TOKENS
~~~text
LowerIdent LowerIdent LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent UpperIdent OpenRound LowerIdent CloseRound OpArrow LowerIdent ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
x 
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **x ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**when_simple.md:1:6:1:8:**
```roc
when x is
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
