# META
~~~ini
description=Example if-then-else statement with a tag expression
type=expr
~~~
# SOURCE
~~~roc
if Bool.True Ok(0) else Err(1)
~~~
# TOKENS
~~~text
KwIf UpperIdent Dot UpperIdent UpperIdent OpenRound Int CloseRound KwElse UpperIdent OpenRound Int CloseRound ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
else 
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **else ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**if_then_else_simple_tag.md:1:20:1:25:**
```roc
if Bool.True Ok(0) else Err(1)
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
