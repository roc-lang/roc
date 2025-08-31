# META
~~~ini
description=minimal reproduction of record parsing index out of bounds crash
type=expr
~~~
# SOURCE
~~~roc
{ i, Complete]
~~~
# TOKENS
~~~text
OpenCurly LowerIdent Comma UpperIdent CloseSquare ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
]
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**fuzz_crash_033.md:1:1:1:14:**
```roc
{ i, Complete]
```
^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **]** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**fuzz_crash_033.md:1:14:1:15:**
```roc
{ i, Complete]
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
