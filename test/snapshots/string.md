# META
~~~ini
description=string
type=expr
~~~
# SOURCE
~~~roc
# TODO: Add Roc code here
~~~
# TOKENS
~~~text
LineComment ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
# TODO: Add Roc code here
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **<unknown>** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.



# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 3
(var #0 _)
(var #1 _)
(var #2 _)
~~~
# TYPES
~~~roc
~~~
