# META
~~~ini
description=Malformed record syntax (error case)
type=expr
~~~
# SOURCE
~~~roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon String Comma OpColon Int Comma Comma LowerIdent OpColon Comma LowerIdent UpperIdent Dot LowerIdent Comma String OpColon LowerIdent Comma Int OpColon String Comma OpColon CloseCurly ~~~
# PARSE
~~~clojure
(malformed)
~~~
# FORMATTED
~~~roc
30
~~~
# EXPECTED
PARSE ERROR - error_malformed_syntax.md:1:18:1:19
PARSE ERROR - error_malformed_syntax.md:1:20:1:22
# PROBLEMS
**UNEXPECTED TOKEN IN EXPRESSION**
The token **: ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**error_malformed_syntax.md:1:18:1:20:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                 ^^


**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**error_malformed_syntax.md:1:1:1:20:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
^^^^^^^^^^^^^^^^^^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **30** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**error_malformed_syntax.md:1:20:1:22:**
```roc
{ name: "Alice", : 30, , email: , active Bool.true, "invalid": value, 42: "number key", : }
```
                   ^^


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
