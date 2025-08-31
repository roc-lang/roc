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


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**record_extension_update.md:1:13:1:16:**
```roc
{ ..person, age: 31, active: True }
```
            ^^^


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
