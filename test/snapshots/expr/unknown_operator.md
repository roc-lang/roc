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
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
2
~~~
# EXPECTED
NIL
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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**unknown_operator.md:1:6:1:7:**
```roc
1 ++ 2
```
     ^


# CANONICALIZE
~~~clojure
(Stmt.malformed)
~~~
# SOLVED
~~~clojure
; No expression to type check
~~~
# TYPES
~~~roc
# No expression found
~~~
