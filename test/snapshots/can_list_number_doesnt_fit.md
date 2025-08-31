# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1u8, 2u8, 300]
~~~
# TOKENS
~~~text
OpenSquare Int LowerIdent Comma Int LowerIdent Comma Int CloseSquare ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
u8
~~~
# EXPECTED
NIL
# PROBLEMS
**LIST NOT CLOSED**
This list is not properly closed.
Expected either a comma **,** to continue the list or a closing bracket **]** to end it.

**can_list_number_doesnt_fit.md:1:1:1:3:**
```roc
[1u8, 2u8, 300]
```
^^


**UNEXPECTED TOKEN IN EXPRESSION**
The token **u8** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**can_list_number_doesnt_fit.md:1:3:1:5:**
```roc
[1u8, 2u8, 300]
```
  ^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**can_list_number_doesnt_fit.md:1:3:1:5:**
```roc
[1u8, 2u8, 300]
```
  ^^


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
