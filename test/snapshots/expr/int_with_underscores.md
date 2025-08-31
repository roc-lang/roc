# META
~~~ini
description=Integer literal with underscores
type=expr
~~~
# SOURCE
~~~roc
1_000_000
~~~
# TOKENS
~~~text
Int Underscore Int Underscore Int ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
_
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **_** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**int_with_underscores.md:1:2:1:3:**
```roc
1_000_000
```
 ^


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
