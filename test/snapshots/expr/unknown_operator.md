# META
~~~ini
description=Unknown operator, should produce an error
type=expr
~~~
# SOURCE
~~~roc
1 ++ 2
~~~
# TOKENS
~~~text
Int OpPlus OpPlus Int ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
2
~~~
# EXPECTED
UNEXPECTED TOKEN IN EXPRESSION - unknown_operator.md:1:4:1:5
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **+ ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unknown_operator.md:1:4:1:6:**
```roc
1 ++ 2
```
   ^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **2** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**unknown_operator.md:1:6:1:7:**
```roc
1 ++ 2
```
     ^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
; Total type variables: 6
(var #0 _)
(var #1 _)
(var #2 _)
(var #3 _)
(var #4 _)
(var #5 _)
~~~
# TYPES
~~~roc
~~~
