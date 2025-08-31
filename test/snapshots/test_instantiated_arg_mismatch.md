# META
~~~ini
description=Type mismatch with instantiated function arguments
type=expr
~~~
# SOURCE
~~~roc
{
    pair : a, a -> (a, a)
    pair = |x, y| (x, y)

    pair(42, "hello")
}
~~~
# TOKENS
~~~text
OpenCurly LowerIdent OpColon LowerIdent Comma LowerIdent OpArrow OpenRound LowerIdent Comma LowerIdent CloseRound LowerIdent OpAssign OpBar LowerIdent Comma LowerIdent OpBar OpenRound LowerIdent Comma LowerIdent CloseRound BlankLine LowerIdent OpenRound Int Comma String CloseRound CloseCurly ~~~
# PARSE
~~~clojure
(malformed malformed:expr_unexpected_token)
~~~
# FORMATTED
~~~roc
pair 
~~~
# EXPECTED
NIL
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: **expected_expr_close_curly**
This is an unexpected parsing error. Please check your syntax.

**test_instantiated_arg_mismatch.md:1:1:2:17:**
```roc
{
    pair : a, a -> (a, a)
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **pair ** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

**test_instantiated_arg_mismatch.md:3:5:3:10:**
```roc
    pair = |x, y| (x, y)
```
    ^^^^^


**UNSUPPORTED NODE**
This syntax is not yet supported by the compiler.
This might be a limitation in the current implementation that will be addressed in a future update.

**test_instantiated_arg_mismatch.md:3:5:3:10:**
```roc
    pair = |x, y| (x, y)
```
    ^^^^^


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
