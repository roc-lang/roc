# META
~~~ini
description=When is old syntax use match instead (should error)
type=expr
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 3 -> 4
~~~
# TOKENS
~~~text
LowerIdent LowerIdent LowerIdent Int OpArrow Int Int OpArrow Int ~~~
# PARSE
~~~clojure
(malformed)
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

**when_with_numbers.md:1:6:1:8:**
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
