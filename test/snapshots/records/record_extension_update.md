# META
~~~ini
description=Record update syntax
type=expr
~~~
# SOURCE
~~~roc
{ ..person, age: 31, active: True }
~~~
# TOKENS
~~~text
OpenCurly DoubleDot LowerIdent Comma LowerIdent OpColon Int Comma LowerIdent OpColon UpperIdent CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
age
~~~
# EXPECTED
UNDEFINED VARIABLE - record_extension_update.md:1:5:1:11
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**record_extension_update.md:1:1:1:13:**
```roc
{ ..person, age: 31, active: True }
```
^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **age** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**record_extension_update.md:1:13:1:16:**
```roc
{ ..person, age: 31, active: True }
```
            ^^^


# CANONICALIZE
~~~clojure
(Expr.malformed)
~~~
# SOLVED
~~~clojure
~~~
# TYPES
~~~roc
# No header found
~~~
