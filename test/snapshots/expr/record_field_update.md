# META
~~~ini
description=Record with field update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31 }
~~~
# TOKENS
~~~text
OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
age
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_field_update.md:1:1:1:13:**
```roc
{ ..person, age: 31 }
```
^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **age** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_field_update.md:1:13:1:16:**
```roc
{ ..person, age: 31 }
```
            ^^^


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
