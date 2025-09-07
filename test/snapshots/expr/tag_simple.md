# META
~~~ini
description=tag_simple
type=expr
~~~
# SOURCE
~~~roc
# TODO: Add Roc code here
~~~
~~~
# TOKENS
~~~text
LineComment MalformedUnknownToken MalformedUnknownToken MalformedUnknownToken ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
# TODO: Add Roc code here
~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**tag_simple.md:2:1:2:2:**
```roc
~~~
```
^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **~** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**tag_simple.md:2:2:2:3:**
```roc
~~~
```
 ^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 4
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
~~~
# TYPES
~~~roc
~~~
